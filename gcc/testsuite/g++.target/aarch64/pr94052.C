/* { dg-do compile } */
/* { dg-additional-options "-O2 -std=gnu++11 -w" } */

namespace c {
typedef int d;
template <typename e> struct f { typedef e g; };
template <bool, typename> struct h;
template <typename e> e aa(typename f<e>::g i) { return i; }
template <typename, typename> struct j {};
template <d, typename> struct k;
template <class l, class m> struct k<1, j<l, m>> { typedef m g; };
template <d n, class l, class m> typename k<n, j<l, m>>::g ab(j<l, m>);
} // namespace c
typedef long d;
typedef char o;
typedef int p;
typedef char q;
typedef int r;
namespace {
struct s;
constexpr d t = 6;
template <typename> class ad {
public:
  static constexpr d u = t;
  d v();
  d x();
  d y();
};
class z : ad<int> {};
struct ae {
  p af;
};
class ag {
public:
  ae ah();
};
} // namespace
typedef __Int32x4_t ai;
typedef struct {
  ai aj[2];
} ak;
typedef int al;
void am(p *a, ai b) { __builtin_aarch64_st1v4si(a, b); }
namespace an {
class ao {
public:
  bool operator==(ao);
  d v();
  d x();
};
class ap : public ad<r> {};
class aq {
public:
  c::j<int, int> ar();
  int as();
  int at();
};
class au {
public:
  virtual d av(d);
  virtual ap aw();
  virtual ag ax();
};
class ay {};
class az {
  virtual void ba(const ay &, const s &);
};
using bb = az;
class bc;
class bd : bb {
  void ba(const ay &, const s &);
  bc *be;
  bc *bf;
  bc *bg;
  aq bh;
  int bi;
  int bj;
  ao bk;
};
namespace bl {
namespace bm {
namespace bn {
class bo;
}
} // namespace bm
} // namespace bl
namespace bn {
template <typename ac = c::h<0, bl::bm ::bn::bo>>
ai bp(ac *, ac *, ac *, al, al, al, d, p);
template <typename ac = c::h<0, bl::bm ::bn::bo>>
ak bq(ac *br, ac *bs, ac *bt, al bu, al bv, al bw, d bx, int, int by) {
  ak{bp(br, bs, bt, bu, bv, bw, bx, by), bp(br, bs, bt, bu, bv, bw, bx, by)};
}
template <typename ac = c::h<0, bl::bm ::bn::bo>>
ak bz(ac *, ac *, ac *, al, al, al &, int, p);
template <int> void ca(p *, const ak &);
template <> void ca<1>(p *buffer, const ak &cb) {
  am(buffer, cb.aj[0]);
  am(buffer + 4, cb.aj[1]);
}
int cc(int, int);
} // namespace bn
class bc {
public:
  virtual au *cd();
};
class ce {
public:
  q *cf();
};
template <d> struct cg {
  template <typename ch> static void ci(ay, z cj, ch ck) { ck(cj); }
};
template <typename ch> void cl(ay w, ch ck) {
  z cj;
  cg<z::u>::ci(w, cj, c::aa<ch>(ck));
}
namespace {
template <typename T1, typename cm, int cn> class co {
public:
  static void convolve(ay, int cs, bc *cp, bc *cq, bc *cr, aq cw, int, ao ct) {
    int by = cp->cd()->ax().ah().af;
    int cu = cq->cd()->ax().ah().af;
    cp->cd()->aw().v();
    int cv = cp->cd()->aw().x();
    cp->cd()->aw().y();
    cp->cd()->aw();
    int da = cr->cd()->aw().x();
    int cx = cq->cd()->aw().x();
    cq->cd()->aw().y();
    int cy = cr->cd()->av(0);
    int cz = cr->cd()->av(1);
    bn::cc(cs, cn);
    int de = c::ab<1>(cw.ar());
    cw.as();
    cw.at();
    ay db;
    ce dc;
    ce dd;
    ce w;
    q *di = w.cf();
    cl(db, [&](z) {
      int df;
      dc;
      di;
      cx;
      auto dg(cu);
      auto dh(cu);
      auto dl(cu);
      for (; cz; df += de) {
        auto br = reinterpret_cast<T1 *>(cv);
        auto bs = reinterpret_cast<T1 *>(cv);
        auto bt = reinterpret_cast<T1 *>(df * ct.x());
        auto dj = reinterpret_cast<cm *>(dd.cf() + da);
        for (int dk; dk < cy; dk += cs, dj += cs)
          if (ct == ao()) {
            auto vres = bn::bz(br, bs, bt, dg, dh, dl, cn, by);
            bn::ca<cn>(dj, vres);
          } else
            bn::bq(br, bs, bt, dg, dh, dl, ct.v(), cn, by);
      }
    });
  }
};
template <typename T1, typename cm>
void bz(ay dm, int cs, bc *cp, bc *cq, bc *cr, aq cw, int dn, ao ct) {
  co<T1, cm, 1>::convolve(dm, cs, cp, cq, cr, cw, dn, ct);
  co<T1, cm, 2>::convolve(dm, cs, cp, cq, cr, cw, dn, ct);
}
} // namespace
void bd::ba(const ay &dm, const s &) {
  bz<o, p>(dm, bi, be, bg, bf, bh, bj, bk);
}
} // namespace an
