// { dg-do compile { target i?86-*-* } }
// { dg-options "-fabi-version=0" }

struct A {
  virtual void a ();
};

struct B {
  virtual ~B ();
};

struct C : public A, public B {
  virtual void c ();
};

struct D : virtual public C {
  virtual void d ();
};

void D::d () {}

// { dg-final { scan-assembler _ZTv0_n20_N1DD1Ev } }
