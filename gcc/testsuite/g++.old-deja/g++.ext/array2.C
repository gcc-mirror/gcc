// { dg-do assemble  }
// { dg-options "-w" }
// { dg-require-effective-target alloca }
// This testcase used to cause a crash on the Alpha.

struct A {
  int i;
  ~A() { }
};

A f (int n)
{
  A a[n];
}
