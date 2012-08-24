namespace N {
  template <typename T>
  struct S {
    void f() {}			// { dg-error "definition" }
  };
}

namespace K {
  template <> void N::S<char>::f() {} // { dg-error "different namespace" }
}
