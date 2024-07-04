// PR c++/103994
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<class>
struct A {
  template<class> using key_arg = int;
};

struct B {
  template<class T>
  void f() {
    using type = A<B>::key_arg<T>;
  }
};
