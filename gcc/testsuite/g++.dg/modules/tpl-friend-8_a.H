// PR c++/100134
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

namespace std {
  template<class T> struct A {
    friend void f(A) { }
  };
}
