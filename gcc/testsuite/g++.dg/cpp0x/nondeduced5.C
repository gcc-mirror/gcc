// PR c++/65707
// { dg-do compile { target c++11 } }

template <int a> struct b {
  typedef int c;
  constexpr operator c() { return a; }
};
template <bool> struct d;
template <typename> struct e : b<true> {};
template <typename, typename = d<true>> struct f;
template <typename g> struct f<g, d<e<g>{}>> {};
template struct f<int>;
