// { dg-do assemble  }
// { dg-options "-w" }
// This testcase used to cause a crash on the Alpha.

struct A {
  int i;
  ~A() { }
};

A f (int n)
{
  A a[n];
}
