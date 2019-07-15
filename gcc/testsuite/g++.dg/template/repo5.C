// PR c++/25625
// { dg-options "-frepo" } 
// { dg-final { cleanup-repo-files } }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

template< typename T, T N > struct integral_c {
  static const T value = N;
  typedef integral_c< T, value + 1 > next;
};
template< typename T, T N > T const integral_c< T, N >::value;
integral_c<int,0> a;

int main () {}
