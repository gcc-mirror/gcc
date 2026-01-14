// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<typename T>
struct S {
  static constexpr auto s = ^^T;
  void foo () { constexpr auto s = ^^T; }
};
template struct S<int>;

constexpr void
bud ()
{
  if consteval { static constexpr auto a = ^^::; }
}
auto bar () { return &bud; }
