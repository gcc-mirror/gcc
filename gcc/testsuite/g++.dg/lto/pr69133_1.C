namespace xercesc_3_1 {
class A {
  virtual void m_fn1();
};
class XMLEntityHandler {
public:
  virtual ~XMLEntityHandler();
  virtual void m_fn2(const int &);
  virtual bool m_fn3();
  virtual void m_fn4();
  virtual int m_fn5() = 0;
  virtual void m_fn6(const int &);
};
class B : A, XMLEntityHandler {};
class C : B {
  void m_fn2(const int &);
  void m_fn6(const int &);
};
void C::m_fn2(const int &) {}
void C::m_fn6(const int &) {}
}

