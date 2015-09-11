// { dg-do compile }
class IObject {
public:
  virtual ~IObject();
};
class A {
  virtual int m_fn1();
};
class B {
public:
  virtual int m_fn2(B) const;
};
class D : IObject, public virtual B {};
class G : public D, A {
public:
  G(A);
};
class F : B {
  friend class C;
};
class C {
  void m_fn3(const IObject &, int &);
  void m_fn4(const B &, int &);
};
A a;
void C::m_fn3(const IObject &, int &p2) {
  G r(a);
  m_fn4(r, p2);
}
void C::m_fn4(const B &p1, int &) {
  F b;
  p1.m_fn2(b);
}

