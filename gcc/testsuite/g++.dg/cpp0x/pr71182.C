// PR c++/71182
// { dg-do compile { target c++11 } }

class A {
  template <typename> void As();
};
template <typename T> class B : A {
  void f() {
    A *g ;
    g ? g->As<T>() : nullptr;
  }
};
