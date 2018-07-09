/* { dg-require-effective-target tls } */

template <__thread int T> struct F {}; // { dg-error "11:storage class specified" }
template <static __thread int T> struct G {}; // { dg-error "11:storage class specified" }
