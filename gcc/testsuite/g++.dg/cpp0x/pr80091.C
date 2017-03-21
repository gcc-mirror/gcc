// { dg-do compile { target c++11 } }

// PR 80091 ICE with member fn call from lambda in template

struct A {
  void m_fn1();
};
template <int> struct B : A {
  void m_fn2() {
    [&] { m_fn1(); };
  }
};
