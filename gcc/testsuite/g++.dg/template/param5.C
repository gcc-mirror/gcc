// Verify top-level cv-qualifiers are dropped when determining the substituted
// type of a non-type template parameter, as per [temp.param]/6.
// { dg-do compile { target c++11 } }

template<class T, T V> decltype(V)& f();
using type = decltype(f<const volatile int, 0>());
using type = int&;
