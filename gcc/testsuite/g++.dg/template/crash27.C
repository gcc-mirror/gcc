// PR c++/18586
template <int> struct A {
  template <int N> int A<N>::i; // { dg-error "" } 
};
