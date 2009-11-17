// PR c++/42058
// { dg-options "" }

struct A {};

struct B
{
  A a;
};

B b[1] = (B[]) { 0 }; // { dg-error "initializer" }
