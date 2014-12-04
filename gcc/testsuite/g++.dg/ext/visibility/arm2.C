// { dg-do compile { target arm_eabi } }
// Class data should be exported.
// { dg-final { scan-not-hidden "_ZTV1S" } }
// { dg-final { scan-not-hidden "_ZTI1S" } }
// { dg-final { scan-not-hidden "_ZTS1S" } }

struct S {
  virtual void f();
};

void S::f() {}
