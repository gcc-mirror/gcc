// { dg-do compile }
// { dg-require-effective-target lto }
// { dg-require-effective-target fpic }
// { dg-require-effective-target c++20 }
// { dg-options "-O3 -flto -shared -fPIC -Wno-deprecated-variadic-comma-omission -Wno-return-type" }

enum ac { ad };
struct b {
  b() { ae(c, c, ad); }
  using d = bool (*)(int &, const int &, ac);
  int c;
  d ae;
};
bool e(int &, const int &, ac) {}
struct h : b {
  h() {}
  template <typename f> h(f) { ae = e; }
};
template <typename aa> struct l {
  aa::ab operator[](int);
};
template <class af> struct m {
  typedef af ab;
};
template <class af> class L : public l<m<af>> {};
enum ag { ah };
class ai;
class aj;
struct ak {
  ag g;
  int al;
  bool am;
  bool an;
};
struct ao {
  int ap();
};
struct aq {
  virtual ag ar() { return as; }
  virtual ak at() { return au; }
  virtual ao av() { return aw; }
  virtual int ax() { return ay; }
  virtual int *az() { return &ba; }
  virtual aq *bb() { return bc; }
  virtual bool bd() { return 0; }
  virtual bool be() { return bf; }
  virtual bool bg() { return ah; }
  bool bh() { return ar(); }
  ag as;
  int ay;
  bool bf;
  ao aw;
  int ba;
  aq *bc;
  ak au;
} *n, *bi, *o, *bj, *p, *bk, *q, *r, *s, *t, *bl, *bm, *u;
struct bn {
  virtual ai *bo() { return nullptr; }
  virtual ai *bp() { return nullptr; }
  virtual aj *bq() { return nullptr; }
};
struct ai : bn {
  virtual aq &by() { return g; }
  virtual ag bz() { return bz(); }
  ao i;
  virtual ao ca() { return i; }
  aq g;
} *bs;
struct aj : ai {
  virtual L<bn *> cb() { return cc; }
  L<bn *> cc;
} a;
struct cd {
  static ai *ce(h = {});
};
L<bn *> br;
int bt, bu, bv, bw, bx;
bool j, k;
struct cf {
  void ch(const int &, ai &);
  virtual void cj(const int &...);
};
void cf::ch(const int &ck, ai &p2) {
  p2.bq()->cb();
  n->at().al || bi->at().an;
  o->at() = bj->at();
  br[1]->bp();
  p->at();
  bk->at() = q->at();
  br[j]->bp();
  cj(ck);
  br[3]->bp()->bp();
  cj(ck);
  br[1]->bo()->by().ar() && br[1]->bo()->by().ax() && r->at().an;
  s->at().al && t->at().am || bl->at().al && bm->at().am || u->at().al;
  bs->by().at() = bs->by().at();
  br[k]->bp();
  cj(ck);
  br[1]->bp();
  cj(ck);
  br[1]->bp()->bp()->bp()->bp();
  cj(ck);
  bs->by().at();
  br[1]->bo()->bz() && bs->by().at().an;
  br[bt]->bo()->ca();
  cj(ck);
  br[bt]->bp();
  {
    aq &g = br[bt]->bo()->by();
    for (; g.ax();)
      br[bt]->bp();
    cj(ck);
    br[1]->bo()->by().ar() && br[1]->bo()->by().ax();
    cj(ck);
  }
  aq &v = bs->by(), &cl = br[0]->bo()->by(), &g = bs->by();
  v.at().g || v.at().g || v.at().g || v.at().g;
  v.av().ap() && v.av().ap() && v.av().ap() && v.av().ap();
  cj(ck);
  p2.by().ar() && v.av().ap();
  cj(ck);
  p2.by().ar() && v.av().ap();
  cj(ck);
  p2.by().ar() && p2.by().ax() && bs->by().av().ap() ||
      p2.by().ax() && bs->by().av().ap();
  v.at();
  cj(ck);
  v.av();
  cj(ck);
  cj(ck);
  bs->by().ar() || bs->by().ar() && bs->by().bg();
  bs->by().ar() || bs->by().ar() && bs->by().ar() && bs->by().ax() ||
      bs->by().ax() && bs->by().ar() || bs->by().ar() && bs->by().bg();
  ai *base = cd::ce(), *ci = br[4]->bp();
  aq *cg = base->by().bh() ? base->by().bb() : nullptr;
  cg ? cg->av() : base->by().av();
  cj(ck);
  cj(ck);
  bs->by().av();
  cd::ce([] {})->by().av();
  cj(ck);
  br[1]->bp();
  cj(ck);
  br[1]->bp();
  cj(ck);
  br[1]->bp();
  cj(ck);
  br[1]->bp();
  cj(ck);
  cj(ck);
  br[1]->bp();
  cj(ck);
  br[0]->bo()->bz() && br[1]->bo()->bz() && br[2]->bo()->bz();
  br[0]->bo()->bz() && br[0]->bo()->bz() && br[1]->bo()->bz() &&
      br[1]->bo()->bz() && br[2]->bo()->bz();
  br[1]->bo()->bz() || br[1]->bo()->bz();
  br[0]->bo()->bz();
  br[2]->bo()->by().av();
  cj(ck);
  br[5]->bo()->by().av();
  cj(ck);
  br[8]->bo()->by().av();
  cj(ck);
  br[bu]->bo()->by().av();
  cj(ck);
  br[bv]->bo()->by().av();
  cj(ck);
  br[bw]->bo()->by().av();
  cj(ck);
  br[bx]->bo()->by().av();
  cj(ck);
  br[5]->bo()->by().av();
  cj(ck);
  br[6]->bo()->by().av();
  cj(ck);
  br[1]->bo();
  cd::ce()->by().bh() ? base->by().bb()->av() : base->by().av();
  cj(ck);
  cj(ck);
  auto cc = p2.bq()->cb();
  cj(ck);
  auto &w = cc[0]->bo()->by();
  auto &x = cc[1]->bo()->by();
  w.ar() && w.ar();
  cj(ck);
  x.ar() || x.ar();
  w.bd();
  bool y = w.be(), z = x.be();
  cj(y);
  x.bd();
  cj(z);
  cj(ck);
  br[1]->bo()->by().az();
  cj(ck);
  cl.ar();
  br[2]->bo()->by().ar();
  cj(0);
  br[3]->bp();
  cj(ck);
  cj(ck);
  cj(ck);
  cj(ck);
  ci->by().ar() != cl.ar();
  ci->bz();
  cj(0);
  br[0]->bo()->by();
}
void cf::cj(const int &...) {}
