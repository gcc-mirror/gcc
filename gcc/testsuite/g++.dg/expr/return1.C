// PR c++/18545

struct A;

A foo()  // { dg-error "" }
{
  A a; // { dg-error "" }
  return a;
}
