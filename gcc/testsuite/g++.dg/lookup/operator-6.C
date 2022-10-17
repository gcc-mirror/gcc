// PR c++/83035
// { dg-do compile { target c++11 } }
// Like operator-3.C but where the lookup occurs at non-block scope.

template<class T, class = void> struct S {
  static constexpr bool is_primary = true;
};

template<class T> struct S<T, decltype(+T())> { };
template<class T> struct S<T, decltype(-T())> { };
template<class T> struct S<T, decltype(*T())> { };
template<class T> struct S<T, decltype(~T())> { };
template<class T> struct S<T, decltype(&T())> { };
template<class T> struct S<T, decltype(!T())> { };
template<class T> struct S<T, decltype(++T())> { };
template<class T> struct S<T, decltype(--T())> { };
template<class T> struct S<T, decltype(T()++)> { };
template<class T> struct S<T, decltype(T()--)> { };

template<class T> struct S<T, decltype(T()->*T())> { };
template<class T> struct S<T, decltype(T() / T())> { };
template<class T> struct S<T, decltype(T() * T())> { };
template<class T> struct S<T, decltype(T() + T())> { };
template<class T> struct S<T, decltype(T() - T())> { };
template<class T> struct S<T, decltype(T() % T())> { };
template<class T> struct S<T, decltype(T() & T())> { };
template<class T> struct S<T, decltype(T() | T())> { };
template<class T> struct S<T, decltype(T() ^ T())> { };
template<class T> struct S<T, decltype(T() << T())> { };
template<class T> struct S<T, decltype(T() >> T())> { };
template<class T> struct S<T, decltype(T() && T())> { };
template<class T> struct S<T, decltype(T() || T())> { };
template<class T> struct S<T, decltype(T(), T())> { };

template<class T> struct S<T, decltype(T() == T())> { };
template<class T> struct S<T, decltype(T() != T())> { };
template<class T> struct S<T, decltype(T() < T())> { };
template<class T> struct S<T, decltype(T() > T())> { };
template<class T> struct S<T, decltype(T() <= T())> { };
template<class T> struct S<T, decltype(T() >= T())> { };
#if __cplusplus > 201703L
template<class T> struct S<T, decltype(T() <=> T())> { };
#endif

template<class T> struct S<T, decltype(T() += T())> { };
template<class T> struct S<T, decltype(T() -= T())> { };
template<class T> struct S<T, decltype(T() *= T())> { };
template<class T> struct S<T, decltype(T() /= T())> { };
template<class T> struct S<T, decltype(T() %= T())> { };
template<class T> struct S<T, decltype(T() |= T())> { };
template<class T> struct S<T, decltype(T() ^= T())> { };
template<class T> struct S<T, decltype(T() <<= T())> { };
template<class T> struct S<T, decltype(T() >>= T())> { };

namespace N { struct A { }; }

#include "operator-3-ops.h"

static_assert(S<N::A>::is_primary, "");
