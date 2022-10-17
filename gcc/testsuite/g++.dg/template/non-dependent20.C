// PR c++/104432
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  template<class=int> static void f(T);
  static void f();
};

template<class> struct B : A<int>, A<int&> {
  using A<int>::f;
  using A<int&>::f;
  void g() { f(0); }
};

template struct B<int>;
