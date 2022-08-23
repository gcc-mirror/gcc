// PR c++/104432
// { dg-do compile { target c++11 } }

struct A {
  template<class=int> void f();
  void f(int);
};

template<class> struct B : A {
  using A::f;
  void g() { f(); }
};

template struct B<int>;
