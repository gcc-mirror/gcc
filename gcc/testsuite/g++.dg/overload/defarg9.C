// PR c++/62115

struct A {};
struct B : A {};

struct C
{
  C(A& a = B()) {}		// { dg-error "rvalue" }
};

C c;				// { dg-error "" }
