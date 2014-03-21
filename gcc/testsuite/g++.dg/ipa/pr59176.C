/* { dg-do compile } */
/* { dg-options "-O3" } */

template <class> class A {
protected:
  void m_fn2();
  ~A() { m_fn2(); }
  virtual void m_fn1();
};

class D : A<int> {};
template <class Key> void A<Key>::m_fn2() {
  m_fn1();
  m_fn1();
  m_fn1();
}

#pragma interface
class B {
  D m_cellsAlreadyProcessed;
  D m_cellsNotToProcess;

public:
  virtual ~B() {}
  void m_fn1();
};

class C {
  unsigned long m_fn1();
  B m_fn2();
  unsigned long m_fn3();
};
unsigned long C::m_fn1() {
CellHierarchy:
  m_fn2().m_fn1();
}

unsigned long C::m_fn3() {
CellHierarchy:
  m_fn2().m_fn1();
}
