/* { dg-do compile } */
typedef long a;
void *b, *c;
template <typename, typename> class d {};
template <typename e, typename f> bool operator!=(d<e, f>, d<e, f>);
class g {
public:
  g(char *);
};
class j {
public:
  j();
  void h();
  void i();
  void aj();
};
class m {
public:
  m(bool);
};
class n {
public:
  operator a();
};
class o {
public:
  long am();
};
class H {
public:
  class p {};
  virtual bool accept(const char *, unsigned long, p *, bool);
};
class q : H {
public:
  class r {
  public:
    enum at { au, av, aw };
  };
  enum { ax };
  virtual void ay(char *, int, const char *, r::at, char *);
  virtual bool az(const g &, unsigned = ax);
  virtual bool ba(const int &, p *, bool);
  void bb(char *bc, long bd, char *, long be) {
    class bf : public p {
    public:
      bf(long);
    } bg(be);
    accept(bc, bd, &bg, true);
  }
};
class s {
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
class w : q {
public:
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
}
