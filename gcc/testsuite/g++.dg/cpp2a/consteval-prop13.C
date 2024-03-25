// P2564R3
// { dg-do compile { target c++20 } }
// Verify we don't recurse endlessly while determining whether a function
// should be propagated to consteval.

consteval int id (int i) { return i; }

constexpr int f2 (auto);

constexpr int
f1 (auto i)
{
  return f2 (i);
}

constexpr int
f2 (auto i)
{
  return f1 (i);
}

auto p = &f1<int>;
auto q = &f2<int>;
