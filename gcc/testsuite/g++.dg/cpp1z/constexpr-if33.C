// PR c++/93324 - ICE with -Wall on constexpr if.
// { dg-do compile { target c++17 } }
// { dg-options "-Wall" }

struct {
  template <int>
  static constexpr bool a() { return 0; }
} e;

template <typename>
void d()
{
  auto c(e);
  using b = decltype(c);
  if constexpr (b::a<2>());
}
