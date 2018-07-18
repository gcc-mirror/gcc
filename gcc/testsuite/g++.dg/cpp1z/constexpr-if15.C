// PR c++/84854
// { dg-options -std=c++17 }

constexpr int foo () { return 1; }
constexpr int foo (int) { return 2; }

template <typename>
void a()
{
  if constexpr(foo) { };
}
