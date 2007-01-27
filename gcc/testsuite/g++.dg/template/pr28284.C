/* { dg-do compile } */

template<int> struct A
{
  static const int i=x; /* { dg-error "was not declared in this scope" } */
  static const int j, k;
};

template<int N> const int A<N>::j = i;
template<int N> const int A<N>::k = j;

A<0> a;
