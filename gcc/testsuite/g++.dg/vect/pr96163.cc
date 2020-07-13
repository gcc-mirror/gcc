// { dg-do compile }
// { dg-require-effective-target c++17 }

typedef double b __attribute__((__vector_size__(16)));
b c;
enum { d };
namespace e {
template <typename> struct g;
struct h {
  enum { i, j };
};
template <typename> struct aa;
} // namespace e
template <typename> struct k;
template <typename> class l;
template <typename, int = e::h::j> class ab;
template <typename, int m, int n, int = 0, int = m, int = n> class o;
template <typename> class ac;
class p;
namespace e {
template <typename> struct q { typedef ac<o<double, 2, 1>> ae; };
struct s {
  template <int, typename t> void af(double *ak, t) { *(b *)ak = c; }
};
} // namespace e
template <typename ad> class ab<ad, d> : public k<ad> {};
template <typename ad> class ab<ad> : public ab<ad, d> {
public:
  typedef typename e::g<ad>::ag ag;
  using ab<ad, d>::ah;
  ag ai() {
    long aj = 0;
    return e::aa(ah()).ai(aj);
  }
  ag operator[](long) { return ai(); }
};
template <typename ad> class ay : public ab<ad> {
public:
  enum { a, al };
};
template <typename ad> class ac : public ay<ad> {
public:
  p am();
};
template <typename> struct k {
  o<double, 2, 1> &ah() { return *static_cast<o<double, 2, 1> *>(this); }
};
namespace e {
template <typename f> struct aa { aa(f); };
template <typename ad> struct aa<l<ad>> {
  typedef ad ao;
  typedef typename ao::ag ag;
  aa(ao &ak) : ap(ak.aq()) {}
  ag &ai(long ak) { return ap[ak]; }
  ag *ap;
};
template <typename ag, int ar, int as, int at, int au, int av>
struct aa<o<ag, ar, as, at, au, av>> : aa<l<o<ag, ar, as>>> {
  typedef o<ag, ar, as> aw;
  aa(aw &ak) : aa<l<aw>>(ak) {}
};
template <typename ax> struct u {
  enum { az, ba, bb };
  static void bc(ax ak) { ak.template be<bb, d, typename ax::bf>(az, ba); }
};
template <typename ax> struct v {
  static void bc(ax ak) { u<ax>::bc(ak); }
};
template <typename bg, typename bh> class w {
  typedef bg bi;

public:
  typedef bg bj;
  typedef bg bf;
  w(bj ak, int, bh, bi x) : bk(ak), bl(x) {}
  template <int bm, int, typename> void be(long, long) {
    bn.template af<bm>(&bk.ai(0), 0);
  }
  bj bk;
  bh bn;
  bi bl;
};
template <typename bi, typename bo, typename bh> void bp(bi &ak, bo, bh bq) {
  typedef aa<bi> bj;
  bo br;
  bj bs(ak);
  typedef w<bj, bh> ax;
  ax bd(bs, br, bq, ak);
  v<ax>::bc(bd);
}
template <typename> struct bt;
template <typename bu, typename bv, typename bw> void bx(bu &ak, bv by, bw bq) {
  bt<bw>::bc(ak, by, bq);
}
template <typename> struct bt {
  static void bc(o<double, 2, 1> &ak, int by, s bq) { bp(ak, by, bq); }
};
} // namespace e
class bz {
public:
  template <typename an> void operator*(an);
};
namespace e {
template <int ca> struct cb { double am[ca]; };
} // namespace e
template <int ca> class cc {
  e::cb<ca> ap;

public:
  double *aq() { return ap.am; }
};
template <typename ad> class l : public e::q<ad>::ae {
public:
  typedef typename e::q<ad>::ae cd;
  typedef typename e::g<ad>::ag ag;
  cc<cd::al> ce;
  ag *aq() { return ce.aq(); }
  l() {}
  template <typename an> l(an ak) { bx(this->ah(), ak, e::s()); }
  template <typename cf, typename cg> void ch(cf, cg by) {
    ag *z, *y;
    { z = aq(); }
    y = z;
    y[0] = aq()[1] = by;
  }
};
namespace e {
template <typename ci, int m, int n, int cj, int ck, int cl>
struct g<o<ci, m, n, cj, ck, cl>> {
  typedef ci ag;
};
} // namespace e
template <typename, int m, int n, int, int, int>
class o : public l<o<double, m, n>> {
public:
  typedef l<o> cd;
  template <typename cf, typename cg> o(cf ak, cg by) { cd::ch(ak, by); }
  template <typename an> o(an ak) : cd(ak) {}
};
class p : public bz {};
double cq;
void cm() {
  o<double, 2, 1> r = 0;
  o cn = r;
  cn.am() * o<double, 2, 1>(0, r[0] / cq).am();
}
