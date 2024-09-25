// PR c++/114683
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M:a }

module;
namespace foo {
  enum class a { x, y, z };
}
export module M:a;
namespace bar {
  export using enum foo::a;
}
