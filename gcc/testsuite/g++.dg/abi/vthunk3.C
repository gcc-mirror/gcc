// { dg-do compile { target i?86-*-* } }
// { dg-options "-fabi-version=0" }

struct A {
  virtual void a ();
};

struct B : virtual public A {
  virtual void b ();
  virtual void a ();
};

struct C {
  virtual void c ();
};

struct D : public C, public B {
};

struct E : virtual public D {
  void b ();
};

void E::b () {}

// { dg-final { scan-assembler _ZTvn4_n20_N1E1bEv } }
