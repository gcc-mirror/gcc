// PR tree-optimization/65747
// { dg-do compile }

struct A {};
struct E {
  virtual A m2();
} *a;
struct B {
  char b[sizeof (E)];
  void m1();
};
struct C {
  B c;
  void m3() { c.m1(); }
  friend class D;
};
struct D {
  int m4(C);
  void m5();
  void m6(int, C);
  void m7(int, C);
  void m8();
  bool m9();
  void m10(int);
  void m11(int);
};
void B::m1() { a = (E *)b; a->m2(); }
void D::m10(int) { m8(); }
void D::m11(int) { m8(); }
int D::m4(C p1) { p1.m3(); return 0; }
void D::m6(int, C p2) {
  int b = 0;
  if (m9()) {
    m4(p2);
    m10(b);
  } else
    m5();
  m10(int());
}
void D::m7(int, C p2) {
  int c = 0;
  if (m9()) {
    m4(p2);
    m11(c);
  } else
    m5();
  m11(int());
}
