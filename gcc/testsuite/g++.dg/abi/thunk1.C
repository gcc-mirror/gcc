// { dg-do compile { target i?86-*-* } }

struct A {
  virtual void f ();
};

struct B : public virtual A {
  virtual void f ();
};

struct C {
  virtual void g ();
};

struct D : public C, public B {
  virtual void f ();
};

void D::f () {}

// { dg-final { scan-assembler _ZThn4_N1D1fEv } }
// { dg-final { scan-assembler _ZTv0_n12_N1D1fEv } }
