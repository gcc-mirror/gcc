// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

namespace hidden {
  struct S {};
  enum E { e };
  void f();
}

export module M;
export namespace exposed {
  using hidden::S;
  using hidden::E;
  using hidden::e;
  using hidden::f;
}
