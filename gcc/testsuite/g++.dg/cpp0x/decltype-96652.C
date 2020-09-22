// PR c++/96652
// { dg-do compile { target c++11 } }

struct A {};

template <typename T>
struct B
{
  A m;
  friend decltype(m);
};

A a;
B<int> b;
