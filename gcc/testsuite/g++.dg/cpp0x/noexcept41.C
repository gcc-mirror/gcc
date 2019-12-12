// PR c++/72845
// { dg-do compile { target c++11 } }

template <typename> struct g { static const int h = 0; };
template <typename i> void declval() { static_assert(!g<i>::h,""); }
template <typename> struct a {
  template <typename d, typename c>
  friend auto f(d &&, c &&) noexcept(declval<c>) -> decltype(declval<c>); // { dg-error "different exception" }
};
template <typename d, typename c> auto f(d &&, c &&) -> decltype(declval<c>);
struct e {};
static_assert((e{}, declval<a<int>>),""); // { dg-error "no context to resolve type" }
