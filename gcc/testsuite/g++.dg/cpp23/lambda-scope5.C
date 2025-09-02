// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++17 } }

constexpr auto equal() {
  int t = 0;
  return [t](auto obj) { return obj; };
}
template <typename F>
constexpr int bar (F) {
  return 42;
}

template <typename F>
constexpr auto compose(F f)
{
  return [f=f](int i) -> decltype(bar (f)) {
      return 42;
  };
}
template <auto> constexpr auto eq = equal();
static_assert(compose(eq<3>)(1));
