// { dg-do compile { target i?86-*-* } }
// { dg-options -w }

struct A {
  virtual void f2 ();
  virtual void f3 ();
};

struct B : virtual public A {
  virtual void f3 ();
};

struct C : public A, public B {
  virtual void f4 ();
};

struct D : virtual public B, virtual public C, virtual public A 
{
  virtual void f5 ();
  virtual void f6 ();
  virtual void f3 ();
};

void D::f3 () {}

// { dg-final { scan-assembler _ZTvn4_n20_N1D2f3Ev } }
