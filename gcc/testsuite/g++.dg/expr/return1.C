// PR c++/18545

struct A;

A foo()
{      // { dg-error "incomplete" }
  A a; // { dg-error "incomplete" }
  return a;
}
