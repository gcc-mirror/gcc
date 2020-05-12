// PR c++/92583
// { dg-do compile { target c++17 } }

template <int> struct a {
  constexpr operator int() { return 42; }
};
template <typename> using b = int;
template <typename d, d> struct e {};
template <typename d, d g> using h = e<d, __integer_pack(g)...>;
template <typename j, typename k, k... index> void apply(j f, e<k, index...>) {
  (f(a<index>{}), ...);
}
template <auto l, typename j> void m(j f) {
  using k = b<decltype(l)>;
  using n = h<k, l>;
  apply(f, n{});
}
template <int, int c> void o() {
  auto p = [](auto i) {
    if constexpr (a<i>{}) ;
    if constexpr (typename a<i>::t{});	// { dg-error "" }
  };
  m<c>(p);
}
auto q() { o<0, 1>; }
