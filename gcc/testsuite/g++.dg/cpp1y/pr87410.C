// PR c++/87410
// { dg-do compile { target c++14 } }

template <long a> using b = const char[a];
template <typename, int c, int e, typename f>
constexpr auto g(b<c> &, b<e> &, f) {}
template <typename d, int a> auto h(b<a> &) {
  auto i = j(static_cast<d **>(nullptr));
  return **i;
}
class k {
  using l = k;
  const int &m() const;
  friend constexpr auto j(l **n) -> decltype(n) {
    g<int>("", "", static_cast<const int &(k::*)() const>(&k::m));
    return n;
  }
};
k o = h<k, 1>("");
