// PR c++/115194
// { dg-additional-options "-fmodules-ts -Wno-global-module" }

module;

namespace NS1 {
  namespace NS2 {
    class Thing {};
  } // NS2
  using NS2::Thing;
} // NS1

export module modA;

export
namespace NS1 {
  using ::NS1::Thing;
  namespace NS2 { }
}
