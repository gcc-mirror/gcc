/* { dg-do compile } */
/* { dg-options "-O" } */

template <class T> void fun(T);
struct B {};
struct R {
  int *x;
  B f;
};
R v(int &, R);
void rfun(R &);
struct A {
  void m_fn2(R p1) {
    R a = p1;
    rfun(p1);
    fun(this);
    fun(a);
  }
};
struct J {
  A ep;
  A ap;
  int c2a;
  void m_fn1(R &p2) {
    R d, e, b;
    v(c2a, p2);
    e = v(c2a, b);
    ap.m_fn2(e);
    v(c2a, p2);
    d = v(c2a, b);
    ep.m_fn2(d);
  }
};
struct N {
  int &p_;
  J cfo;
};
void fn3(N&n) {
  R h;
  n.cfo.m_fn1(h);
}
extern N &c;
void fn1() { fn3(c); }
