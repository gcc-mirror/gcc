// PR target/88785
// { dg-do compile }
// { dg-options "-O2 -g -std=c++17 -mavx512vl -mavx512dq" }

namespace a {
template <class> class b;
template <class> class d;
}
template <int f> struct g { static constexpr int e = f; };
template <typename> struct aa;
template <typename...> struct o;
template <typename h, typename ab> struct o<h, ab> : aa<h>::ac {};
template <typename...> struct j;
template <typename h, typename ab> struct j<h, ab> : aa<ab>::ac {};
template <typename... k> constexpr bool l = o<k...>::e;
template <typename, typename> struct r : g<false> {};
template <typename m> struct r<m, m> : g<true> {};
template <typename ad> struct aa { typedef ad ac; };
template <bool, typename ad, typename> using ae = ad;
template <typename...> using af = void;
typedef float ag __attribute__((__vector_size__(16)));
ag ah;
ag ai(__attribute__((__vector_size__(2 * sizeof(long long)))) long long z) {
  ah = ag{};
  __attribute__((__vector_size__(4 * sizeof(float)))) float aj = ah;
  return __builtin_ia32_cvtqq2ps128_mask(z, aj, 1);
}
namespace a {
int ak;
int al;
template <long> int am;
template <class> struct an;
template <class> struct ao;
template <class m> using ap = typename ao<m>::ac;
template <class, unsigned long, class = af<>> struct aq;
template <class m> using ar = aq<m, sizeof(m)>;
template <class as, class at> as au(at);
template <class> struct av;
template <class m> using aw = typename av<m>::ac;
template <class m, int ax> struct ay {
  static constexpr long az = ax * sizeof(int);
  using ac [[gnu::__vector_size__(az)]] = m;
};
template <class m, long ba> using bb = typename ay<m, ba>::ac;
template <class m> struct p {
  using bc = decltype(m()[0]);
  static constexpr int bd = sizeof(bc);
};
template <class as, class m, class = p<m>> as be(m);
template <class as, class... at> as bf(at... z) { return be<as>(z...); }
template <class m, unsigned long ax, class bg = aw<m>> bg bh(aq<m, ax>) {
  return bg();
}
template <> struct av<float> {
  using ac [[gnu::__vector_size__(16)]] = float;
};
template <class> struct av {
  using ac [[gnu::__vector_size__(16)]] = long long;
};
template <unsigned long bi> struct aq<bool, bi> {};
template <class m, unsigned long bi, class bj = bb<m, bi>,
          bool = l<r<bb<m, bi>, aw<m>>, r<bj, m>>>
struct bk;
template <class m, unsigned long bi, class bj> struct bk<m, bi, bj, true> {
  bj bl;
  bk(bb<m, bi> z) : bl(z) {}
};
template <class m, unsigned long bi, class bj> struct bk<m, bi, bj, false> {};
template <class m, unsigned long bi> struct aq<m, bi> : bk<m, bi> {
  using bm = bb<m, bi>;
  static constexpr long bd = bi;
  aq();
  template <class bn> aq(bn z) : bk<m, bi>(z) {}
  m operator[](long);
};
template <class> constexpr long bo = g<0>::e;
template <class m> struct ao { using ac = typename an<m>::br; };
template <class bp, class m> class bq {
  using bu = m;
  bp bs;
  m bt;

public:
  template <class bn, class bv> void q(bn z, bv bw) {
    auto s = bx(bt), by = bx(bs);
    ap<bu>::bz(s, z, bw, by);
  }
};
class ca {
public:
  template <class bn, class bv> void cb(bn, bv);
};
template <class m> ca cc(typename b<m>::cd, m &);
template <class m> bq<d<m>, b<m>> cc(typename b<m>::cd, const m &);
struct ce;
template <class t, int ax> struct cf {
  using br = typename t::br;
  using cg = aq<long, ax>;
  using ch = aq<long, ax>;
};
struct ci {
  template <class m> static constexpr long cj = sizeof(m);
  struct ck : j<int, g<sizeof(int)>> {};
  template <class> static constexpr bool cl = ck::e;
  using br = ce;
  template <class m> using cn = ae<cl<m>, cf<ci, cj<m>>, int>;
};
template <class> struct an : ci::cn<long> {};
template <class> class d : an<int> {
  using cm = ch;

public:
  cm bl;
};
template <class m> auto bx(m z) { return z.bl; }
template <class> class b : an<int> {
  using cm = cg;

public:
  using cd = d<long>;
  static long cu();
  b();
  template <class bn, class bv> b(bn, bv) {}
  cm bl;
};
template <class m> auto bx(b<m> z) { return z.bl; }
template <class m, class co> void cq(ar<m>, co, aq<bool, ar<m>::bd>);
template <class as, class bu, class cp> as be(bu z) {
  using cs = typename cp::bc;
  constexpr long ax = cp::bd;
  auto cr = bh(z);
  aq<cs, ax> f;
  using bn = typename p<as>::bc;
  constexpr bool cy = sizeof(f), ct = sizeof(bn);
  if (ct)
    if (cy) {
      ag cw = ai(cr);
      return cw;
    }
}
template <class as, class at> auto cv(at z) { return bf<as>(z); }
struct G {
  template <class m> using ch = typename ci::cn<m>::ch;
  template <class m, unsigned long ax, class bn, class co>
  static void bz(aq<m, ax> z, bn *, co, ch<m> cx) {
    using da = aq<bn, sizeof(bn)>;
    using bu = typename da::bm;
    using bp = aq<ae<1, bool, bn>, da::bd>;
    auto cz = cv<bu>(z);
    cq(da(cz), ae<0, int, co>(), au<bp>(cx));
  }
};
struct ce : G {};
}
class D {
public:
  D(...);
  template <typename db> void operator<<(db);
};
template <class dc> dc dd;
struct de {
  long cu();
};
template <typename dc> void dg() {
  using db = long;
  auto df = dd<a::b<long>>;
  using a::ak;
  using a::al;
  constexpr long di = 1, alignment = a::bo<a::b<long>>;
  using dh = ae<di, decltype(al), int>;
  dh dk, am = a::am<alignment>;
  const a::b<long> dj;
  de u;
  auto dm = 0 ? u.cu() : 0;
  float dl[dm];
  db reference;
  a::b<long> x;
  auto compare = [&](long) {
    int n;
    a::b<long>(reference, ak);
    for (auto i = 0; 0; ++i)
      [] {
        auto v = 0, w = 0;
        return D(w, v);
      }() << n;
  };
  compare(0);
  using c = a::b<long>::cd;
  c dn;
  a::b y = df;
  auto v(y);
  cc(dn, x).cb(dl, am);
  long i;
  cc(dn, dj).q(&dl[dc::cu()], dk);
  ++i;
}
void test() { dg<a::b<long>>(); }
