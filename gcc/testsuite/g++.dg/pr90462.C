/* { dg-options "-Wdeprecated-copy -fno-diagnostics-json-formatting -fdiagnostics-format=json" } */

template <class> class b;
struct B {
  typedef b<char> *c;
};
class d {
public:
  B::c operator->();
};
template <class> struct e;
class f {
  typedef int g;
};
template <class, class> class h;
template <class i> class b {
public:
  i j;
  i k;
  int l;
  void assign() {
    int m;
    h<i, int> n(&m);
    n.o(&j, &k, l);
  }
};
template <class i, class> class s : f { s &p(const i *, const i *, g); };
template <class i, class t> s<i, t> &s<i, t>::p(const i *, const i *, g) {
  d q;
  q->assign();
}
struct G {
  G();
  G(int);
  G(G &);
};
template <class i, class> class h {
public:
  h(int *);
  void o(const i *, const i *, unsigned);
  i r();
};
template <class i, class t> void h<i, t>::o(const i *, const i *, unsigned) {
  G a;
  a = r();
}
template s<char, e<char>> &s<char, e<char>>::p(const char *, const char *, g);

/* { dg-regexp ".*" } */
