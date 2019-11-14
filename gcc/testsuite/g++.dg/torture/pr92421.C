// PR ipa/92421
// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

typedef long a;
void *b, *c;
template <typename, typename> class d {};
template <typename e, typename f> bool operator!=(d<e, f>, d<e, f>);
struct g {
  g(const char *);
};
struct j {
  j();
  void h();
  void i();
  void aj();
};
struct m {
  m(bool);
};
struct n {
  operator a();
};
struct o {
  long am();
};
struct H {
  class p {};
  virtual bool accept(const char *, unsigned long, p *, bool);
};
struct q : H {
  struct r {
    enum at { au, av, aw };
  };
  enum { ax };
  virtual void ay(const char *, int, const char *, r::at, const char *);
  virtual bool az(const g &, unsigned = ax);
  virtual bool ba(const int &, p *, bool);
  void bb(char *bc, long bd, char *, long be) {
    struct bf : public p {
      bf(long) {}
    } bg(be);
    accept(bc, bd, &bg, true);
  }
};
struct s {
  q *bi;
  bool bj();
};
template <class bk> class t : q {
  bool accept(const char *, unsigned long bd, p *bg, bool) {
    bool k(bp || bq), cl = false, err = false;
    if (br)
      ay("", 1, __func__, r::au, "");
    if (bs)
      ay("", 6, __func__, r::av, "");
    char bt[1], cd[1];
    long bu = sizeof(int) + bd, ce = sizeof(L) + bd;
    char *bw = bu > sizeof(bt) ? new char : bt,
         *cf = ce > sizeof(cd) ? new char : cd;
    __builtin___memcpy_chk(b, c, bd, 0);
    a by[1];
    int bz = 0;
    u cb = *cc((int *)bw, true, by, &bz);
    ay("", 1, __func__, r::aw, "");
    if (bw != bt)
      delete bw;
    __builtin___memcpy_chk(b, c, bd, 0);
    cb.ch.i();
    bool ci = cj((L *)cf, bg);
    bool atran = bq && bp && cb.ck;
    if (atran && !ci && cm(&cb))
      if (cn > co) {
        int cp = cb.cq % 6;
        v cs = *(ct + cp);
        if (cu(&cs))
          cl = true;
      }
    if (ci)
      if (k)
        cv.aj();
    cv.h();
    b = cc((int *)bw, false, by, &bz);
    if (b)
      if (cw(&cb, by, bz))
        if (atran && bp && cx())
          cv.aj();
    if (cl)
      if (k)
        cv.aj();
    cv.h();
    int cp = cb.cq % 6;
    v cs = *(ct + cp);
    if (cy())
      err = true;
    O da = *(db + cp);
    if (da.dc->am() > cs.dc->am() + cs.dd->am() + 1 && de(&da))
      cv.aj();
    return !err;
  }
  bool ba(const int &, p *, bool) {
    d<int, int> kit, df;
    while (kit != df)
      ;
    cx();
    return false;
  }
  bool az(const g &, unsigned) {
    t dj;
    int cur;
    while (cur) {
      int dk, dl;
      char dbuf;
      dj.bb(&dbuf, dl, &dbuf, dk);
    }
  }
  struct L {};
  struct u {
    j ch;
    a cq;
    bool ck;
  };
  struct v {
    o *dd;
    o *dc;
  };
  struct O {
    o *dc;
  };
  bool cy();
  bool cu(v *);
  bool cj(L *, p *);
  bool de(O *);
  u *cc(int *, bool, a *, int *);
  bool cw(u *, a *, int);
  bool cx() {
    dm.dn();
    bool err = false;
    if (dm.l())
      err = true;
    return !err;
  }
  bool cm(u *);
  j cv;
  int br;
  bool bs;
  bool bq;
  bk dm;
  a co;
  n cn;
  v ct[6];
  O db[6];
  bool bp;
};
struct w : q {
  void dn();
  bool l() {
    m(true);
    if (br)
      ay("", 1087, __func__, r::au, "");
    return false;
  }
  int br;
};
bool s::bj() {
  bi->az("");
  new t<w>;
  return false;
}
