// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;
namespace A {
  enum E1 { Exposed_1, Hidden_1 };
  enum E2 { Exposed_2, Hidden_2 };
}
export module M;
namespace A {
  export using E1::Exposed_1;
}
namespace B {
  export using A::E2::Exposed_2;
}
