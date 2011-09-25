// { dg-options -std=c++0x }

struct A
{
  int i = f();
  static int f(int i = 42) { return i; }
};

A a;
