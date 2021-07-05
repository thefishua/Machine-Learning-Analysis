:- op(300, xfx, <-).
% Example
inter_construction(C1 <- B1, C2 <- B2, C1 <- Z1B, C2 <- Z2B, C <- B) :-
    C1 \= C2, % Checks if heads are different
    intersection(B1, B2, B), % Finds the intersections of the two lists
    gensym(z, C), % Binds C to z
    subtract(B1, B, B11), % Subtracts the intersection from the list
    subtract(B2, B, B12),  % Subtracts the intersection from the list
    append(B11, [C], Z1B), % Appends the subtraction to Z1B
    append(B12, [C], Z2B). % Appends the subtraction to Z2B

%Question 2.1 (2 marks):
intra_construction(C1 <- B1, C2 <- B2, C1 <- ZB, C <- Z1B, C <- Z2B) :-
    C1 == C2,
    intersection(B1, B2, B), % Finds the intersections of the two lists
    gensym(z, C), % Binds C to z
append(B, [C], ZB), % Appends the Substitute to the head
    subtract(B1, B, Z1B), % Subtracts the intersection from the list
    subtract(B2, B, Z2B).  % Subtracts the intersection from the list

% Intra_Construction tests

% Test 1
% Input
% intra_construction(x <- [b, c, d, e], x <- [a, b, d, f], X, Y, Z).

% Output
% X = x<-[b, d, z1],
% Y = z1<-[c, e],
% Z = z1<-[a, f].

test_intra_construction_1(X, Y, Z) :- intra_construction(x <- [b, c, d, e], x <- [a, b, d, f], X, Y, Z).

% Test 2 TODO
% Input
% intra_construction(x <- [], x <- [], X, Y, Z).

% Output
% X = x<-[z1],
% Y = Z,
% Z = z1<-[].

test_intra_construction_2(X, Y, Z) :- intra_construction(x <- [], x <- [], X, Y, Z).

% Test 3
% Input
% intra_construction(x <- [a, b, c], x <- [a, b, c], X, Y, Z).

% Output
% X = x<-[a, b, c, z1],
% Y = Z,
% Z = z1<-[].

test_intra_construction_3(X, Y, Z) :- intra_construction(x <- [a, b, c], x <- [a, b, c], X, Y, Z).

%Question 2.2 (2 marks):
% Y is always a subset of X
absorption(C1 <- B1, C2 <- B2, C1 <- Z1B, C2 <- B2)  :-
    C1 \= C2, % Checks if heads are different
    cmp_len(B1, B2, R1, R2),
    intersection(R1, R2, B), % Finds the intersections of the two lists
    subtract(R1, B, N1), % Subtracting intersection from both lists
    append([C2], N1, Z1B).

% Absorption Tests
 
% Test 1
% Input
% absorption(x <- [a, b, c, d, e], y <- [a, b, c], X, Y).

% Output
% X = x<-[y, d, e],
% Y = y<-[a, b, c].

test_absorption_1(X, Y):- absorption(x <- [a, b, c, d, e], y <- [a, b, c], X, Y).

% Test 2
% Input
% absorption(x <- [], y <- [], X, Y).

% Output
% X = x<-[y],
% Y = y<-[].
test_absorption_2(X, Y):- absorption(x <- [], y <- [], X, Y).

% Test 3
% Input
% absorption(x <- [a, b, c, d, e], y <- [a, b, c, d, e], X, Y).

% Output
% X = x<-[y, d, e],
% Y = y<-[a, b, c].
test_absorption_3(X, Y):- absorption(x <- [a, b, c, d, e], y <- [a, b, c, d, e], X, Y).

%Question 2.3 (2 marks):
identification(C1 <- B1, C2 <- B2, C1 <- Z1B, C3 <- Z2B)  :-
    C1 == C2,
    intersection(B1, B2, B), % Finds the intersections of the two lists
    subtract(B1, B, R1),
subtract(B2, B, R2),
    member(X, R2), % From subtracting the one element that is different in the list gets the member of that list
    C3 = X, % The new list head is the one element that is different
    append(B, R2, Z1B),
    append(R1, [], Z2B).

% Base Case where both lists r empty
%identification(C1 <- [], C2 <- [], C1 <- Z1B, C2 <- Z1B) :-
%    append([], [], Z1B).
 
% Identification Tests

% Test 1
% Input
% identification(x <- [a, b, c, d, e], x <- [a, b, y], X, Y).

% Output
% X = x<-[a, b, y],
% Y = y<-[c, d, e].

test_identification_1(X, Y) :- identification(x <- [a, b, c, d, e], x <- [a, b, y], X, Y).
% Test 2 TODO
% Input
% identification(x <- [], x <- [], X, Y).

% Output
% X = x<-[],
% Y = X.
test_identification_2(X, Y) :-identification(x <- [], x <- [], X, Y).

% Test 3
% Input
% identification(x <- [a, b, c, d, e], x <- [a, b, z], X, Y).

% Output
% X = x<-[a, b, z],
% Y = z <- [c, d, e].
test_identification_3(X, Y) :- identification(x <- [a, b, c, d, e], x <- [a, b, z], X, Y).

% Question 2.4 (2 marks):
% dichotomisation(x <- [a,b,c,d], not(x) <- [a,c,j,k], A, B, C, D).
dichotomisation(C1 <- B1, C2 <- B2, C1 <- ZB, C2 <- Z1B, C <- Z2B, C3 <- Z3B) :-
    C1 \= C2, % head doesnt equal the other head
intersection(B1, B2, B), % Intersection of both heads
gensym(z, C),
append(B, [C], ZB),
    append(B, [not(C)], Z1B),
    subtract(B1, B, D),
    append(D, [], Z2B),
    C3 = not(C),
    subtract(B2, B, E),
    append(E, [], Z3B).

% Dichotomisation Tests
% Test 1
% Input
% dichotomisation(x <- [a,b,c,d], not(x) <- [a,c,j,k], A, B, C, D).

% Output
% A = x<-[a, c, z1],
% B = not(x)<-[a, c, not(z1)],
% C = z1<-[b, d],
% D = not(z1)<-[j, k].
test_dichotomisation_1(A, B, C, D) :- dichotomisation(x <- [a,b,c,d], not(x) <- [a,c,j,k], A, B, C, D).

% Test 2 TODO
% Input
% dichotomisation(x <- [], not(x) <- [], A, B, C, D).

% Output
% A = x<-[],
% B = not(x)<-[],
% C = z1<-[],
% D = not(z1)<-[].
test_dichotomisation_2(A, B, C, D) :- dichotomisation(x <- [], not(x) <- [], A, B, C, D).
% Test 3
% Input
% dichotomisation(x <- [a,b,c,d, e, y], not(x) <- [a,c,j,k, y], A, B, C, D).

% Output
% A = x<-[a, c, y, z1],
% B = not(x)<-[a, c, y, not(z1)],
% C = z1<-[b, d, e],
% D = not(z1)<-[j, k].

test_dichotomisation_3(A, B, C, D) :- dichotomisation(x <- [a,b,c,d,e,y], not(x) <- [a,c,j,k,y], A, B, C, D).

%Question 2.5 (2 marks):
truncation(C1 <- B1, C2 <- B2, C1 <- Z1B)  :-
    C1 == C2,
    intersection(B1, B2, Z1B).
% Truncation Tests

% Test 1
% Input
% truncation(x <- [a, b, c, d], x <- [a, c, j, k], X).

% Output
% X = x<-[a, c].
test_truncation_1(X) :- truncation(x <- [a, b, c, d], x <- [a, c, j, k], X).

% Test 2
% Input
% truncation(x <- [], x <- [], X).

% Output
% X = x<-[].

test_truncation_2(X) :- truncation(x <- [], x <- [], X).

% Test 3
% Input
% truncation(x <- [a, b, c], x <- [a, c, b], X).

% Output
% X = x<-[a, c].

test_truncation_3(X) :- truncation(x <- [a, b, c], x <- [a, c, b], X).

%======================= Helper Functions ============================

% This was actually not needed as Y is always a subset of X
len([], X) :-
    X = 0.
len([_ | T], Count):-
    len(T, Prev),
    Count is Prev + 1.
cmp_len(L1, L2, L11, L12):-
    len(L1, C1),
    len(L2, C2),
    C1 > C2 -> append([], L1, L11), append([], L2, L12);
    append([], L1, L12), append([], L2, L11).
