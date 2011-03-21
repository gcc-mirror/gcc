// PR c++/45651

namespace { template <int T> struct A {}; }
template <int T> struct B { void f(A<T>); };
template struct B<1>;
template<int T> void B<T>::f(A<T>) {}
