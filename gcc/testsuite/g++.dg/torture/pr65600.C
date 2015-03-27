// { dg-do compile }
// { dg-options "-std=c++11" }
class A {
public:
  virtual void m_fn1() {}
  int weak_release___trans_tmp_1;
  void m_fn2() {
    __asm__("\n\n\n\n");
    if (weak_release___trans_tmp_1)
      m_fn1();
  }
};
class weak_count {
  A *pi_;

public:
  weak_count() : pi_() {}
  ~weak_count() {
    if (pi_)
      pi_->m_fn2();
  }
};
class B {
  weak_count pn;
};
int
main() { B a; }

