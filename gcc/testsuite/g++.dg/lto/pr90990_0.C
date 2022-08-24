// { dg-lto-do link }
/* { dg-extra-ld-options {  -r -nostdlib } } */
// { dg-require-effective-target lto_incremental }
class A {
public:
  float m_floats;
  A() {}
};
class B {
public:
  A operator[](int);
};
class C {
  B m_basis;

public:
  A operator()(A) {
    m_basis[1] = m_basis[2];
    A a;
    return a;
  }
};
class D {
public:
  C m_fn1();
};
class F {
  A m_pivotInB;
  F(D &, const A &);
};
F::F(D &p1, const A &p2) : m_pivotInB(p1.m_fn1()(p2)) {}

