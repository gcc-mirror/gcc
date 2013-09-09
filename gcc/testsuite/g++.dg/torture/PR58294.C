// { dg-do compile }
struct A {
  virtual ~A();
  virtual void m_fn1() { delete this; }
  void m_fn2() { m_fn1(); }
};

struct B {
  A *pi_;
  B() { pi_->m_fn2(); }
};
struct C {
  B pn;
};
void _setjmp();
int png_decode() {
  _setjmp();
  C a;
  return 0;
}
