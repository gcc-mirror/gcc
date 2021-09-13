// PR tree-optimization/98282
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

template <typename> struct g;
template <typename b> struct g<b &> { typedef b c; };
template <typename b> typename g<b>::c &&d(b &&e) {
  return static_cast<typename g<b>::c &&>(e);
}
void *operator new(__SIZE_TYPE__, void *f) { return f; }
struct h;
struct k {
  using i = h *;
};
struct D {
  k::i j;
};
struct p : D {
  p(p &&) : D() {}
};
struct r {
  using l = int;
  r(r &&) : ad() {}
  l *ad;
};
struct s {
  static s m();
};
struct t {
  template <typename ah> void operator=(ah);
};
struct I {
  template <typename o> void q(o ai) {
    *ai = aj();
    s::m();
  }
  h aj();
};
template <typename...> class as;
struct J {
  int a;
  char av;
};
template <typename...> struct aw : J {
  void ax(...) {}
};
template <typename... ay, typename an, typename... n>
struct aw<as<ay...>, an, n...> : aw<as<ay...>, n...> {
  using az = as<ay...>;
  using ba = aw<az, n...>;
  char bb;
  an &bc() { return *reinterpret_cast<an *>(this); }
  void ax(az *bd) {
    if (bb)
      new (bd) an(d(bc()));
    ba::ax(bd);
  }
};
template <typename... n> struct as : aw<as<n...>, n...> {
  as();
  as(as &&be) { be.ax(this); }
  void operator=(as be) { be.ax(this); }
};
struct h {
  as<decltype(nullptr), r, p> bg;
};
using bh = t;
struct u {
  bh bj;
};
I bk();
template <typename> void bl() {
  h a;
  bk().q(&a);
}
template <typename> void bn(int) {
  u b;
  b.bj = bl<int>;
}
void bp() { bn<int>(0); }
