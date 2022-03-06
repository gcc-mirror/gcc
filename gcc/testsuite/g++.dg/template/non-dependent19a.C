// PR c++/104432
// { dg-do compile { target c++11 } }
// A variant of non-dependent19.C where A is a template.

template<class>
struct A {
  template<class=int> void f();
  void f(int);
};

template<class> struct B : A<int> {
  using A<int>::f;
  void g() { f(); }
};

template struct B<int>;
