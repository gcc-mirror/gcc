// PR c++/114868
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M:a }

module;
namespace foo {
  void a();
}
export module M:a;

namespace bar {
  // propagate usings from partitions
  export using foo::a;
}
