// PR c++/65175
// { dg-do compile { target c++11 } }

template <class a> using au = typename a::av;
template <bool, class a> using az = a;
template <template <class...> class b, class... bf> struct d {
  template <class bh> struct f { typedef bh e; };
  static auto g(int) -> f<b<bf...>>;
  typedef typename decltype(g(0))::e e;
};
template <class = void> class h;
template <class, class, template <class...> class, class...> struct i;
struct j {
  typedef int bu;
};
namespace bv {
template <class> struct k : j {};
template <class bx> struct l : i<int, void, k, bx> {};
} // namespace bv
template <class bx> auto bw(bx) -> az<bv::l<bx>::c, typename bv::l<bx>::bz>;
template <class, class cd, class> struct n : j { au<cd> av; };
template <class, class cd, class> struct K : j { au<cd> av; };
template <class cj> struct o {
  typedef typename cj::e e;
  static const bool c = true;
  typedef h<typename e::bu> bz;
};
template <class, class, template <class...> class cj, class... bf>
struct i : o<d<cj, bf...>> {};
template <class> class h {
public:
  typedef int av;
  struct : i<int, int, K, int, h, int> {} s;
  template <class> struct p : i<int, int, n, int, h<h>, int> {};
  template <class bx, class cl>
  auto m_fn2(bx, cl) -> az<p<cl>::c, typename p<cl>::bz>;
  template <class bx> static auto q(int, int, bx cm) -> decltype(bw(cm));
};
int a, b;
void c() { h<>::q(b, 5, a).m_fn2(5, a); }
