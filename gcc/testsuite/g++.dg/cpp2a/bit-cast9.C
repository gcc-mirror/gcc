// PR c++/98469
// { dg-do compile { target c++20 } }
// { dg-options "-Wall" }

template<typename T, typename F>
constexpr T
bit_cast (const F &f) noexcept
{
  return __builtin_bit_cast (T, f);
}
struct S { int s; };
constexpr int foo (const S &x) { return x.s; }
constexpr int bar () { return foo (bit_cast<S> (0)); }
constexpr int x = bar ();
static_assert (!x);
