// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

// Don't get confused by these valid cases.

template <class>
class A {
  template <bool> void b();
  void m_fn1();
};

template <class T>
void A<T>::m_fn1() { b<>(0); }


template <int> struct X {
  X() { fn<>(0); }
  template <int> void fn();
};


template <typename> void a() { a<int>; }
