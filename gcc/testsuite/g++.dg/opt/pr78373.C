// PR c++/78373
// { dg-do compile { target c++11 } }

struct A {
  static A singleton;
};
struct B {
  void m_fn2();
  virtual int m_fn1();
};
struct D : B {
  static int m_fn3(int, int, int, A) {
    D &self = singleton;
    self.m_fn2();
  }
  static D singleton;
};
template <typename, typename> struct C { bool m_fn4() const; };
template <typename Base, typename Traits> bool C<Base, Traits>::m_fn4() const {
  Traits::m_fn3(0, 0, 0, Base::singleton);
}
template struct C<A, D>;
