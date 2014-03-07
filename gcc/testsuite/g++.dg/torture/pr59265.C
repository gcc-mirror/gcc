// { dg-do compile { target c++11 } }
// { dg-options "-fprofile-use" }

class A {
  int m_fn1() const;
  unsigned m_fn2() const;
};
class B {
public:
  virtual void m_fn1();
};
class C final : B {
  C();
  virtual void m_fn2() { m_fn1(); }
};
int a;
unsigned A::m_fn2() const {
  if (m_fn1())
    return 0;
  a = m_fn2();
}
C::C() {}
