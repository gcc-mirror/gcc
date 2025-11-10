// PR c++/100134
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

namespace std {
  template<class T> struct A {
    friend void f(A) { }
  };
}

namespace outer {
  namespace inner {
    inline namespace more_inner {
      template<class T> struct B {
	friend void g() {}
      };
    }
  }
}
