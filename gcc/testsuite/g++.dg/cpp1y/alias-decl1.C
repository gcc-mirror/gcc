// DR 1710 - Missing template keyword in class-or-decltype
// { dg-do compile { target c++14 } }

template <int> struct S {
  template <int> struct A;
  template <int N> using U = typename A<N>::foo;
};
template <typename T> typename S<1>::U<T::foo>::type a;
template <typename T> typename S<1>::template U<T::foo>::type a2;
