// PR c++/89612
// { dg-do compile { target c++17 } }

template <typename a> using b = typename a ::c;
template <typename> bool d;
template <typename, typename> struct e {
  template <typename f, typename g> e(f, g) {}
  template <typename h, typename i, typename j>
  friend auto k(h &&, const j &, i &&) noexcept(d<b<h>, h> &&d<b<i>, i>);
};
template <typename l, typename m> e(l, m)->e<l, m>;
template <typename l, typename m, typename j>
auto k(l &&, const j &, m &&) noexcept(d<b<l>, l> &&d<b<m>, m>);
int main() {
  e(0, [] {});
}
