// { dg-do compile { target arm*-*-eabi* arm*-*-symbianelf* } }
// { dg-options "-fvisibility=hidden" }
// Most class data should be exported.
// { dg-final { scan-not-hidden "_ZTV1S" } }
// { dg-final { scan-not-hidden "_ZTI1S" } }
// { dg-final { scan-not-hidden "_ZTS1S" } }
// { dg-final { scan-not-hidden "_ZTV1U" } }
// { dg-final { scan-not-hidden "_ZTT1U" } }
// { dg-final { scan-not-hidden "_ZTI1U" } }
// { dg-final { scan-not-hidden "_ZTS1U" } }
// The construction vtable should be hidden.
// { dg-final { scan-hidden "_ZTC1U0_1T" } }

struct S {
  virtual void f();
};

void S::f() {
}

struct T : public virtual S {
  virtual void g();
};

struct U : public virtual T {
  virtual void h();
};

void U::h() {}
