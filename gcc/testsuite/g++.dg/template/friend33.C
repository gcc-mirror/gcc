// { dg-do compile }
// PR c++/18733: Validation of template headers in friends

template<int> struct A
{
  void foo();
};

struct B
{
  friend void A<0>::foo();
};
