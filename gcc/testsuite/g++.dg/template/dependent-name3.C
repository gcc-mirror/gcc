// { dg-do compile }

// Dependent arrays of invalid size generate appropriate error messages

template<int I> struct A
{
  static const int zero = 0;
  static const int minus_one = -1;
};

template<int N> struct B
{
  int x[A<N>::zero];       // { dg-error "zero" }
  int y[A<N>::minus_one];  // { dg-error "size .-1. of array is negative|narrowing conversion|not an integral constant-expression" }
};

B<0> b;
