// Test that late-parsed default args have the same consteval semantics.
// { dg-do compile { target c++20 } }

template <int N>
consteval bool foo (bool x) { if (x) throw N; return false; }
consteval bool qux (bool x) { if (x) throw 1; return false; }
template <int N>
consteval bool bar (bool x = foo<N> (true)) { return true; }
template <int N>
consteval bool corge (bool x = qux (true)) { return true; }
template <int N>
struct S
{
  consteval static bool baz (bool x = foo<N> (true)) { return true; }
  consteval static bool garply (bool x = qux (true)) { return true; }
};
struct T
{
  template <int N>
  consteval static bool baz (bool x = foo<N> (true)) { return true; }
  template <int N>
  consteval static bool garply (bool x = qux (true)) { return true; }
};
constexpr bool a = bar<0> (true);
constexpr bool b = corge<0> (true);
constexpr bool c = S<0>::baz (true);
constexpr bool d = S<0>::garply (true);
constexpr bool e = T::baz<0> (true);
constexpr bool f = T::garply<0> (true);
