// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

namespace foo {
  struct S {} S;
}

export module M;

namespace bar {
  export using foo::S;
  export using X = struct foo::S;
}
