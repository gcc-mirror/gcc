// PR c++/42058
// { dg-options "" }

struct A;

struct B
{
  A a; // { dg-error "incomplete type" }
};

B b[1] = (B[]) { 0 };
