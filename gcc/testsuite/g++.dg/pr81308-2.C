/* { dg-do compile } */
/* { dg-options "-w -O2" } */

struct A {
  int operator[](int) const {}
};
struct B {
  void m_fn1();
};
struct C {
  virtual bool m_fn2(int, unsigned &, A &, int &, unsigned long &, bool);
};
template <class MCAsmParserImpl> struct D {
  D(int) { MCAsmParserImpl(0, 0, 0, 0); }
};
int a;
namespace {
struct F : C {
  bool m_fn2(int, unsigned &, A &, int &, unsigned long &, bool);
  unsigned m_fn3(const A &, B &);
  F(int, int, int, int) {}
};
}
bool F::m_fn2(int, unsigned &, A &p3, int &, unsigned long &, bool) {
  B b;
  m_fn3(p3, b);
}
void fn1() { D<F>(0); }
unsigned F::m_fn3(const A &p1, B &p2) {
  for (int *p;; p++)
    switch (*p) {
    case 0:
      p1[a];
    case 1:
      p2.m_fn1();
    }
}

