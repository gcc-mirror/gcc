// PR c++/28595

template<int> struct A
{
  static const int i;
};

template<int N> struct B
{
  char c[A<N>::i], d; // { dg-error "constant" }
};

B<0> b;
