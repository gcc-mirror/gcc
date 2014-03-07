// { dg-do compile { target c++11 } }

struct A
{
  int i = f();
  static int f(int i = 42) { return i; }
};

A a;
