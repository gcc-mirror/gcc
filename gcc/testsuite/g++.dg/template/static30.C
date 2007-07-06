// PR c++/31992

template <int> struct A 
{
  static const int i1;
  static const int i2;
};

template <int N> const int A<N>::i1(A<N>::i);
template <int N> const int A<N>::i2(3, A<N>::i);
