// PR c++/89422
// { dg-do compile { target c++11 } }
// { dg-additional-options -g }

template <int> struct S
{
  friend void foo (int a = []{ return 0; }()) {}
  int b;
};
S<0> t;
