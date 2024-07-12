// PR c++/115798
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi base }

module;
#include <cstdint>
export module base;

export {
  using ::int8_t;
}

export namespace std {
  using std::int8_t;
}
