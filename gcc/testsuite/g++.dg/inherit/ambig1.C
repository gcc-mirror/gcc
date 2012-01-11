// PR c++/51614

struct A
{
  void foo();
};

struct B : A {};
struct C : A {};

struct D : B, C
{
  D() { A::foo(); }		// { dg-error "ambiguous" }
};
