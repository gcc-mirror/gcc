// PR c++/121724
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi foo }

module;
namespace A {
  inline namespace X {
    namespace B {
    }
  }
}
export module foo;
export namespace A {
  namespace B {
    struct S {};
  }
}
