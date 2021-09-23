/* PR target/99842 */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O3 -mdejagnu-cpu=power10 -Wno-return-type" } */

/* Verify we do not ICE on the following noisy creduced test case.  */

enum { a, b, c, d };
template <typename> struct e;
template <typename g, typename h, typename k> struct e<g(h, k)> {
  typedef h f;
};
template <typename> struct ac;
template <typename ab> struct ac<const ab> : ac<ab> {};
template <typename> struct l;
template <typename, int, int m, int = 0, int = a, int = m> class n;
template <typename> class o;
template <typename, typename, typename> class ag;
template <typename, typename, int = c> class af;
template <typename> struct ad;
template <typename ab> struct an {
  typedef n<typename ab ::ah, ac<ab>::ai, ac<ab>::aj> f;
};
template <typename al> struct am { typedef o<al> f; };
template <typename al, typename = typename ac<al>::ao,
          typename = typename ac<al>::av>
struct ak;
template <typename al, typename ao> struct ak<al, ao, int> {
  typedef typename am<al>::f f;
};
template <typename, typename, typename> struct aq;
template <typename ar, typename as> struct aq<ar, ar, as> { typedef ar at; };
template <typename ap> ap bf(const typename ad<ap>::f *);
template <typename ap, int> ap aw(typename ad<ap>::f *ax) { return bf<ap>(ax); }
typedef __attribute__((altivec(vector__))) double au;
template <> struct ad<au> { typedef double f; };
template <> au bf(const double *ax) { return __builtin_vec_vsx_ld(0, ax); }
template <typename> struct az {};
template <typename al> class o : public l<al> {
public:
  typedef typename ac<al>::ah ah;
  template <typename ay> al &operator+=(const o<ay> &);
};
template <typename> struct l {};
template <typename ba, typename bb, int bd> struct ac<af<ba, bb, bd>> {
  typedef typename ba::ah ah;
  enum { ai, aj };
};
template <typename, typename, int bd>
class af
    : public ak<
          af<ag<int, const n<double, -1, -1, 3>, const n<double, -1, -1, 3>>,
             n<double, -1, 1, 3>, bd>,
          int, int>::f {};
template <typename, typename, typename> struct be;
template <typename bj, typename bg, typename g> void bi(bj, bg bm, g) {
  typename an<bg>::f bk(bm);
}
template <typename bj, typename bg, typename g> void bl(bj, bg bm, g bp) {
  be<bj, bg, g>::bn(a, bm, bp);
}
template <typename, typename, typename, typename> struct bo;
class bs {
public:
  bs(double *, int);
  double &operator()(int, int) { return bq[br]; }
  template <typename bw, int> bw bt(int i, int j) {
    double &bu = operator()(i, j);
    return aw<bw, b>(&bu);
  }
  double *bq;
  int br;
};
class ca : public bs {
public:
  ca(double *by, int bz) : bs(by, bz) {}
};
template <typename al> class ce : public am<al>::f {
protected:
  template <typename ay> void cb(l<ay>) {
    af<ag<int, const n<double, -1, -1, 3>, const n<double, -1, -1, 3>>,
       n<double, -1, 1, 3>>
        cc;
    bl(0, cc, az<typename ay::ah>());
  }
  template <typename> void ch(long);
  template <typename ay> void ch(l<ay> cf) { cb(cf); }
};
template <typename cg, int aa, int m, int cl, int ci, int cj>
struct ac<n<cg, aa, m, cl, ci, cj>> {
  typedef cg ah;
  typedef int av;
};
template <typename cg, int, int m, int, int, int>
class n : public ce<n<cg, m, c>> {
public:
  template <typename ab> n(ab p) { n::template ch<ab>(p); }
};
template <typename bc, typename ba, typename bb> struct ac<ag<bc, ba, bb>> {
  typedef ba ao;
  typedef typename e<bc(typename ba::ah, typename bb::ah)>::f ah;
  typedef typename aq<typename ac<ba>::av, typename ac<bb>::av, bc>::at av;
};
template <typename> class cm;
template <typename, typename r, typename cs>
class ag
    : public cm<typename aq<typename ac<r>::av, typename ac<cs>::av, int>::at> {
};
template <typename>
class cm : public ak<ag<int, n<double, 1, 1>, n<double, 1, 1>>>::f {};
template <typename al>
template <typename ay>
al &o<al>::operator+=(const o<ay> &) {
  af<ag<int, const n<double, -1, -1, 3>, const n<double, -1, -1, 3>>,
     n<double, -1, 1, 3>>
      co;
  bi(0, co, int());
}
enum { cp };
template <int> struct cq;
template <typename> struct cr {
  enum { q };
  enum { ae = cq<q>::at };
};
template <> struct cq<cp> {
  enum { at = d };
};
struct t {
  template <typename ba, typename bb, typename s> static void bn(ba, bb, s) {
    typedef typename bb::ah x;
    x u;
    bo<long, ca, x, ca>::bn(0, 0, ca(0, 0), ca(&u, 1), 0, 0, 0);
  }
};
template <typename, typename bb, int = cr<bb>::ae> struct cu;
template <typename cd, typename ba, typename bb, int ct, typename ah>
struct be<cd, af<ba, bb, ct>, az<ah>> {
  static void bn(cd, af<ba, bb> bm, az<ah>) {
    ag<int, const n<double, -1, -1, 3>, const n<double, -1, -1, 3>> da;
    cu<ba, bb>::cv(c, da, bm);
  }
};
template <typename al> struct cw {
  template <typename bj>
  static void
  cv(bj, ag<int, const n<double, -1, -1, 3>, const n<double, -1, -1, 3>>,
     n<double, -1, 1, 3> bx) {
    double alpha;
    ag<int, const n<double, -1, -1, 3>, const n<double, -1, -1, 3>> bh;
    al::cx(c, bh, bx, alpha);
  }
};
template <typename ba, typename bb> struct cu<ba, bb, d> : cw<cu<ba, bb>> {
  template <typename s> static void cx(s, ba, bb bx, typename af<ba, bb>::ah) {
    ba cz;
    t::bn(cz, bx, c);
  }
};
template <typename dj, typename, bool>
void db(__vector_quad *, __vector_pair &, dj);
template <typename, typename, typename, typename, typename, typename, int>
void dc(ca alhs) {
  typedef au dj;
  typedef au dd;
  ca bh(alhs);
  enum { de };
  __vector_quad df, dg;
  int j;
  dd v;
  __vector_pair dh;
  __builtin_mma_assemble_pair(
      &dh, (__attribute__((altivec(vector__))) char)bh.bt<dj, de>(0, j),
      (__attribute__((altivec(vector__))) char)bh.bt<dj, de>(0, j));
  db<dj, dd, true>(&df, dh, v);
  __vector_pair di;
  __builtin_mma_assemble_pair(
      &di, (__attribute__((altivec(vector__))) char)bh.bt<dj, de>(0, j),
      (__attribute__((altivec(vector__))) char)bh.bt<dj, de>(0, j));
  db<dj, dd, true>(&dg, di, v);
}
template <typename bv, typename w, typename cy> struct bo<bv, w, double, cy> {
  static void bn(bv, bv, w bh, cy, double, bv, double) {
    dc<bv, double, w, double, cy, double, d>(bh);
  }
};
void dm() {
  n<double, 1, 1> dk(1), y(0);
  y += dk;
}
