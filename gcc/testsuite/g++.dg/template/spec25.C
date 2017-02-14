namespace N {
  template <typename T>
  struct S {
    void f() {}
  };
}

namespace K {
  template <> void N::S<char>::f() {} // { dg-error "namespace" }
}
