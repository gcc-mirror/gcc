// PR c++/90227
// { dg-do compile { target c++14 } }

template <int b> struct c { static constexpr int d = b; typedef c e; };
template <bool, typename a> using f = a;
template <bool g> using h = c<g>;
template <int g> using i = c<g>;
template <typename...> struct ab {};
template <typename... j> struct k { using e = ab<j...>; };
template <typename> struct ad;
template <typename j, typename... l> struct ad<ab<j, l...>> { using e = ab<l...>; };
template <typename> struct ae;
template <typename... m> struct ae<ab<m...>> : i<sizeof...(m)> {};
template <typename af, int, int = ae<af>::d> struct ag;
template <typename... m> struct ag<ab<m...>, 0, 0> { using e = ab<>; };
template <typename... m, int ah>
struct ag<ab<m...>, 0, ah> : k<typename ag<typename ad<ab<m...>>::e, ah - 1>::e> {};
template <typename... m, int ai, int ah>
struct ag<ab<m...>, ai, ah> : ag<typename ad<ab<m...>>::e, ai - 1> {};
template <typename, template <typename> class> struct aj;
template <typename... j, template <typename> class ak> struct aj<ab<j...>, ak> {
  using e = ab<typename ak<j>::e...>;
};
template <unsigned long> struct an;
struct ao { typedef an<0> ap; };
template <typename a, a...> struct aq {};
template <typename, typename = ao::ap> struct as;
template <typename a, unsigned long... at> struct as<a, an<at...>> { typedef aq<a> ap; };
template <typename a, a> using au = typename as<a>::ap;
template <long... at> using av = aq<unsigned long, at...>;
template <long ar> using aw = au<unsigned long, ar>;
struct ay { using e = h<c<false>::d>; };
template <typename, template <typename> class, typename...>
struct bb : ay::e {};
struct bd { using e = av<>; };
struct bg { using e = bd::e; };
namespace bi {
enum bj { bk };
struct bo { enum n { bp }; };
struct bq { bool br; static const bo::n bs = bo::bp; };
template <typename bv> struct bw { using e = bv; };
template <typename, bj, int, typename...> class bx;
template <typename bv, bj by, int bz, typename... j, long... anchors>
struct bx<bv, by, bz, ab<j...>, av<anchors...>> : bo {
  static const n bs = bv::bs;
  static const long ca = sizeof bv::br;
  using cb = int;
  using cc = ab<j...>;
  using cd = typename ag<cc, bz>::e;
  using ce = typename ag<cc, bz>::e;
  using cf = aw<ca>;
  using cg = typename bw<bv>::e;
  using ch = decltype(cg()(cb(), cd(), ce(), cf()));
};
class ck;
template <typename> struct cl : c<false> {};
template <typename bv, bj by, int bz, typename... j>
struct bx<bv, by, bz, ab<j...>> : public bx<bv, by, bz, typename aj<ab<j...>, cl>::e, bg::e> {};
}
using bi::bj;
using bi::ck;
template <typename cn> class co {
  template <typename p> co(p) { c<cn::bs>(); }
  static co o;
};
namespace bi {
template <typename> class cp;
template <typename j> using cq = bb<j, cp>;
template <typename, typename cr> void cs(cr, f<cq<cr>::d, void *> = nullptr);
}
using bi::cs;
struct cu : bi::bq {
  template <typename... cw, typename... cx>
  auto operator()(int q, ab<cw...>, ab<cx...>, av<>) {
    cs<cw...>(q);
  }
};
template <>
co<bi::bx<cu, bj::bk, 1, ab<int, ck>>>
co<bi::bx<cu, bj::bk, 1, ab<int, ck>>>::o(0);
