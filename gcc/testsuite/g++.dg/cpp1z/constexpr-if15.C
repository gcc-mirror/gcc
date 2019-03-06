// PR c++/84854
// { dg-do compile { target c++17 } }

constexpr int foo () { return 1; }
constexpr int foo (int) { return 2; }

template <typename>
void a()
{
  if constexpr(foo) { };	// { dg-error "overloaded" }
}
