// { dg-additional-options "-fmodules-ts -fdump-lang-module -Wno-global-module" }
// { dg-final { scan-lang-dump {Writing definition '::foo::bar::baz'} module } }

module;
namespace foo {
  namespace bar {
    struct baz { };
  }
  using bar::baz;
}
export module foo;
namespace foo {
  export using foo::baz;
}
