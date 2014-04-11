// DR 1467, c++/51747
// { dg-do compile { target c++11 } }

struct X { };

X x;
X x2{x};
