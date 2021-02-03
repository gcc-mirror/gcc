/* { dg-do compile } */
/* { dg-require-effective-target c++11 } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=z13" { target s390x-*-* } } */

template <int a> struct k { static constexpr int c = a; };
template <bool, bool, typename...> struct o;
template <typename f, typename... g> struct o<false, false, f, g...> {
  typedef decltype(0) h;
};
template <typename... g> struct p : o<k<false>::c, k<false>::c, g...> {};
class q;
class r {
public:
  void ap(q);
};
template <typename, typename aw> void ax(aw ay) { ay(); }
template <typename az> void ba(az bb) {
  using bc = p<az>;
  using bd = typename bc::h;
  ax<bd>(bb);
}
template <unsigned> class s;
class t {
public:
  s<8> br();
  template <typename...> void operator()() { ba(br()); }
};
class q {
public:
  template <typename az> q(az) { H(); }
  struct H {
    t cc;
    H() { cc(); }
  };
};
template <unsigned long> struct I {};
template <unsigned long cl, typename j> void cm(j cn, I<cl>) {
  cm(cn, I<cl - 1>());
  cn(cl);
}
template <typename j> void cm(j, I<0>) {}
template <unsigned co> struct u {
  long cp[co];
  void cq(const u &);
  void cs(int);
  void operator<(u);
};
template <unsigned co> void u<co>::cq(const u &l) {
  cm([&](int i) { cp[i] &= l.cp[i]; }, I<co>());
}
template <unsigned co> void u<co>::cs(int m) {
  cm([&](int i) { cp[i] >>= m; }, I<co - 2>());
}
template <unsigned> class K;
template <unsigned co> class v {
  int cv;
  friend K<co>;

public:
  void cx(int, unsigned char *, unsigned long long);
};
template <unsigned co> class K {
public:
  static void cx(v<co> &);
};
template <unsigned co>
void v<co>::cx(int, unsigned char *, unsigned long long) {
  K<co>::cx(*this);
}
template <unsigned co> void K<co>::cx(v<co> &cz) {
  u<co> a, b, d;
  int e, n = cz.cv;
  for (; e;)
    if (cz.cv)
      a.cs(cz.cv);
  a.cq(d);
  a < b;
}
template <unsigned co> class s {
  v<co> *dh;

public:
  void operator()();
};
template <unsigned co> void s<co>::operator()() {
  int f;
  unsigned char g;
  long h;
  dh->cx(f, &g, h);
}
void d() {
  r i;
  t j;
  i.ap(j);
}
