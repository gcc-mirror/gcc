// PR c++/97804
// { dg-do compile { target c++17 } }

template <typename a> struct b {
  constexpr b() : c() {}
  [[no_unique_address]] a c;
};
template <unsigned long, typename...> struct d;
template <unsigned long e, typename a, typename... f>
struct d<e, a, f...> : d<1, f...>, b<a> {};
template <unsigned long e, typename a> struct d<e, a> : b<a> {};
template <typename... g> class h : d<0, g...> {};
struct i {};
class j {
  using k = int;
  h<k, i> l;
  float m = 0.025f;
} n;
