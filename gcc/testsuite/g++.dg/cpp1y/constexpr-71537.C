// PR c++/71537
// { dg-do compile { target c++14 } }

constexpr bool
foo ()
{
  constexpr int n[42] = { 1 };
  constexpr int o = n ? 1 : 0;
  constexpr int p = n + 1 ? 1 : 0;
  constexpr int q = "abc" + 1 ? 1 : 0;
  return o + p + q == 3;
}

static_assert (foo (), "");
