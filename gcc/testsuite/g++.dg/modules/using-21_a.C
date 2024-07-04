// { dg-additional-options "-fmodules-ts -Wno-global-module" }

module;
namespace foo {
  void baz();
}
export module foo;
namespace foo {
  export using foo::baz;
  export using foo::baz;
}
