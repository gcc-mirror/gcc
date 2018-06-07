// { dg-do compile { target c++11 } }

template <Preconditioner> class A {  // { dg-error "declared" }
  template <class = int> void m_fn1() {
    m_fn1();
    }
};
