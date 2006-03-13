// PR c++/25625
// { dg-options "-frepo" } 
// { dg-final { cleanup-repo-files } }

template< typename T, T N > struct integral_c {
  static const T value = N;
  typedef integral_c< T, value + 1 > next;
};
template< typename T, T N > T const integral_c< T, N >::value;
integral_c<int,0> a;

int main () {}
