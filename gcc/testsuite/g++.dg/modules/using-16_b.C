// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;
namespace bar {
  void f(int);
}
export module M;
export import :S;
namespace foo {
  export using bar::f;
}
