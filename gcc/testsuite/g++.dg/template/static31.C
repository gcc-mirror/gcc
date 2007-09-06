// PR c++/32674

class C
{
  static const int j = 3;
};

template<int> class A
{
  static const int i1;
  static const int i2;
  static const int i3;
  static const int i4;
};

template<int N> const int A<N>::i1(C::j);
template<int N> const int A<N>::i2 = C::j;
template<int N> const int A<N>::i3(C::j, 5); // { dg-error "compound expression" }
template<int N> const int A<N>::i4 = (C::j, 7);
