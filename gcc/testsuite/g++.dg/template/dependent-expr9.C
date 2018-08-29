// PR c++/69753

class A {
public:
  template <class> void m_fn1();
};
A *fn1(int *);
template <typename> class B : A {
  static int *m_fn2() { fn1(m_fn2())->m_fn1<A>(); return 0; }
};
