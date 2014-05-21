// PR c++/58704
// { dg-do compile { target c++11 } }

struct A {};

template<typename> struct B
{
  A a[1] = { };
};

B<int> b;
