//PR c++/28741

template<void> struct A         // { dg-error "not a valid type" }
{
  static int i;
};

A<0> a;
