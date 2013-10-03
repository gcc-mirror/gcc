// PR c++/58584
// { dg-do compile { target c++11 } }

struct A
{
  int i alignas(this);  // { dg-error "17:invalid use of 'this'" }
};

template<int> struct B
{
  int j alignas(this);  // { dg-error "17:invalid use of 'this'" }
};
