// PR c++/93907
// { dg-options -std=gnu++20 }

// This testcase is a variadic version of concepts-using2.C; the only
// difference is that 'cd' and 'ce' are now variadic concepts.

template <int a> struct c {
  static constexpr int d = a;
  typedef c e;
};
template <typename> struct f;
template <typename b> using g = typename f<b>::e;
struct b;
template <typename b> struct f { using e = b; };
template <typename ai> struct m { typedef g<ai> aj; };
template <typename b> struct n { typedef typename m<b>::aj e; };
template <typename b> using an = typename n<b>::e;
template <typename> constexpr bool ao = c<true>::d;
template <typename> constexpr bool i = c<1>::d;
template <typename> concept bb = i<b>;
#ifdef __SIZEOF_INT128__
using cc = __int128;
#else
using cc = long long;
#endif
template <typename...> concept cd = bb<cc>;
template <typename... bt> concept ce = requires { requires cd<bt...>; };
template <typename bt> concept h = ce<bt>;
template <typename bt> concept l = h<bt>;
template <typename> concept cl = ao<b>;
template <typename b> concept cp = requires(b j) {
  requires h<an<decltype(j.begin())>>;
};
struct o {
  template <cl b> requires cp<b> auto operator()(b) {}
};
template <typename b> using cm = decltype(o{}(b()));
template <typename bt> concept ct = l<bt>;
template <typename da> concept dd = ct<cm<da>>;
template <typename da> concept de = dd<da>;
struct {
  template <de da, typename b> void operator()(da, b);
} di;
struct p {
  void begin();
};
template <typename> using df = p;
template <int> void q() {
  df<int> k;
  int d;
  di(k, d);
}
