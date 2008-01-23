// PR c++/28560

template<typename> struct A {};

template<int> struct B;

template<int N> struct C :
  A<typename B<N>::X __attribute__((unused))> {}; // { dg-warning "attribute" }
