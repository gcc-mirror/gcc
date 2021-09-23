// PR c++/99331
// { dg-do compile { target c++11 } }
// { dg-options "-Wconversion" }
// Don't issue -Wconversion warnings for value-dependent expressions.

template <int> struct X {};
template <signed char> struct Y {};
template <typename T> X<sizeof(T)> foo();
template <typename T> X<alignof(T)> foo2();
template<int I> Y<I> foo3();
template<int> Y<1024> foo4(); // { dg-error "narrowing conversion" }
template<int> Y<1u> foo5();
template<int> X<__INT_MAX__ + 1U> foo6(); // { dg-error "narrowing conversion" }

template <typename T>
struct S {
  using t = X<sizeof(T)>;
  using u = X<alignof(T)>;
};
