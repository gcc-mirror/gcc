// Tests for the fix for PR debug/49366.
struct B {
  void g();
  virtual void v() = 0;
  virtual void w();
};
void B::g() {}
void B::w() {}
struct S : B {
  void f();
  void v();
  void u();
};
void S::f() {}
void S::v() {}
template <void (B::*MF)()> void t() {}
template <void (S::*MF)()> void t() {}
