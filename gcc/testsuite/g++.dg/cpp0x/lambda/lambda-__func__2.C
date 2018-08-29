// PR c++/82029
// { dg-do compile { target c++11 } }

template <typename> struct A {
  void m_fn1() {
    [] { return __func__; }();
  }
};
struct B {
  A<int> a;
  void m_fn2();
};
void B::m_fn2() { a.m_fn1(); }
