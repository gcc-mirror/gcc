// { dg-do compile { target i?86-*-* } }
// { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } }

struct c0 {
  virtual void f ();
};

struct c1 : virtual public c0 {
};

struct c2 : virtual public c0, public c1 {
  virtual void f ();
};

void c2::f () {}

// { dg-final { scan-assembler _ZTv0_n12_N2c21fEv } }
