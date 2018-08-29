/* { dg-do compile } */
class A {
public:
  A(int, int);
};
class B {
public:
  void m_fn1(bool, const int *, int &);
  unsigned m_fn2();
};
namespace {
class C {
  B &VTables;
  void m_fn3(A, unsigned, const int *, int &);

public:
  int VFTableBuilder_VisitedBases;
  B VFTableBuilder_VTables;
  C() : VTables(VFTableBuilder_VTables) {
    m_fn3(A(0, 0), 0, 0, VFTableBuilder_VisitedBases);
  }
};
}
int a;
void C::m_fn3(A, unsigned, const int *, int &) {
  for (;;)
    1 ? VTables.m_fn2() : 0;
}
void B::m_fn1(bool, const int *, int &) { C(); }
unsigned B::m_fn2() { m_fn1(0, 0, a); return 0; }
