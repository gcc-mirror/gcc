// PR c++/58568
// { dg-do compile { target c++11 } }

template<int> struct A
{
  static const int i;
  template<int N> const int A<N>::i = []{ return 0; }(); // { dg-error "29:invalid use" }
};
