// PR rtl-optimization/91164
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdelete-dead-exceptions -fnon-call-exceptions -fno-rerun-cse-after-loop -fno-tree-forwprop" }

template <typename, typename = int> class b;
template <int v> struct d { static constexpr int e = v; };
template <bool, typename f> using g = f;
struct h { template <typename i> using j = i; };
template <typename, typename f> using k = h::j<f>;
void *operator new(__SIZE_TYPE__, void *);
struct l { l(); };
struct m;
template <typename n> n aa(m);
struct o { template <typename> using ab = l; };
template <typename, typename> struct b {
  struct q : o::ab<int> { q(int, l = l()) : p() {} int p; } ac;
  void ad();
  b() : ac(0) {}
  ~b() { bool r = ac.p == 0; if (r) ad(); }
  const wchar_t *ae();
};
struct m {};
struct t { virtual void f(); };
struct u { l a; };
struct af : t {
  struct ag { ag(l); };
  af(l ah) : ai(ah) {}
  ag ai;
};
struct w {
  template <typename f, typename x> w(f, x y) { new (0) af(y.a); }
};
struct z {
  using aj = int;
  template <typename x> z(x ah) : ak(al, ah) {}
  aj al;
  w ak;
};
struct am : z { template <typename x> am(x ah) : z(ah) {} };
template <typename, typename x> am an(x) { return u{}; }
template <typename> am ao() { return an<int>(l()); }
struct ap {
  k<int, int> aq;
  k<int, int> ar;
  k<int, int> as;
};
struct at { ap a; long au; ap av; ap aw; };
struct ax { at c; ax() : c() {} };
enum ay : int;
ay az, ba;
struct bb { bb(wchar_t *, wchar_t *, ay, m); };
template <typename bc> struct bd {
  typedef typename bc::be *bf;
  bd(bf, bf, const typename bc::bg &, ay);
  ay bh;
  bb bi;
  am bj;
  typename bc::bk e;
  ax bl;
  int bm;
};
template <typename, typename> using bn = g<d<false>::e, am>;
template <typename bc>
bd<bc>::bd(bf ah, bf y, const typename bc::bg &bu, ay)
    : bi(ah, y, bh, bu), bj(ao<bc>()), bm(aa<int>(bu)) {}
struct bt { typedef wchar_t be; typedef b<be> bk; typedef m bg; };
template <typename bc, typename bo> bn<bo, bc> bar();
template <typename bc, typename bo> bn<bo, bc> bq(bo) {
  typename bc::bg bp;
  auto bs = nullptr;
  using br = bd<bc>;
  br(bs, bs, bp, ba);
  return bar<bc, bo>();
}
struct bw {
  bw();
  template <typename bv, typename x> void assign(b<bv, x> ah) {
    const wchar_t by = *ah.ae();
    bw(&by, ah.ae(), bp, az);
  }
  template <typename bo> bw(bo, bo y, m, ay) : automaton(bq<bt>(y)) {}
  m bp;
  am automaton;
};
void bx() {
  b<wchar_t> s;
  bw ca;
  ca.assign(s);
}
