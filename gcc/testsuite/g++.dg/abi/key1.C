// On ARM EABI platforms, key methods may never be inline.
// { dg-do compile { target arm*-*-eabi* arm*-*-symbianelf* } }
// { dg-final { scan-assembler-not _ZTV1S } }
// { dg-final { scan-assembler-not _ZTV1T } }
// { dg-final { scan-assembler _ZTV1U } }

struct S {
  virtual void f();
};

inline void S::f() {}

struct T {
  virtual void g();
  virtual void h();
};

inline void T::g() {}

struct U {
  virtual void i();
  virtual void j();
};

inline void U::i() {}
void U::j () {}
