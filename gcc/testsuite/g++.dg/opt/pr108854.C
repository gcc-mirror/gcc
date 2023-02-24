// PR middle-end/108854
// { dg-do compile { target c++11 } }
// { dg-options "-O3" }
// { dg-additional-options "-fPIC" { target fpic } }

struct A { A (int); ~A (); };
struct B { B (int, bool); ~B (); };
template <typename T>
struct C { void m1 (T); void m2 (T &&); };
class D;
struct E { virtual void m3 (); };
template <typename>
struct F { virtual bool m4 (D &); };
struct D { virtual D m5 () { return D (); } };
void foo (void *, void *);
struct G {
  int a;
  C <D *> b;
  void m4 (D &r) { B l (a, true); r.m5 (); b.m1 (&r); b.m2 (&r); }
};
struct H : E, F <int> {
  template <typename T>
  H (int, T);
  bool m4 (D &r) { A l (a); b.m4 (r); if (c) return true; } // { dg-warning "control reaches end of non-void function" }
  int a;
  bool c;
  G b;
};
inline void bar (F <int> &p) { D s, t; p.m4 (t); foo (&p, &s); }
enum I { I1, I2 };
template <I>
struct J;
template <class, class T, class, class, class, class>
void baz () { int g = 0, h = 0; T i (g, h); bar (i); }
template <class, int, I T>
void qux () { baz <int, H, int, int, E, J<T>> (); }
void corge () { qux <int, I2, I1> (); qux <int, I2, I2> (); }
