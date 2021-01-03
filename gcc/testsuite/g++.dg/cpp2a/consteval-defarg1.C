// Test that late-parsed default args have the same consteval semantics.
// { dg-do compile { target c++20 } }

consteval bool foo (bool x) { if (x) throw 1; return false; }
consteval bool bar (bool x = foo (true)) { return true; }
struct S
{
  consteval static bool baz (bool x = foo (true)) { return true; }
};
constexpr bool a = bar (true);
constexpr bool b = S::baz (true);
