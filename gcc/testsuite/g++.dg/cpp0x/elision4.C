// PR c++/107085
// { dg-do compile { target c++11 } }

struct X {
  X();
  X(X&&);
};
struct Z : X {};
X x1 = Z();
X x2 = X(Z());

struct B { };
struct D : B { };
B b1 = D();
B b2 = B(D());
