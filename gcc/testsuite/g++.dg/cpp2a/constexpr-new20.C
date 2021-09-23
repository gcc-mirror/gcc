// PR c++/101663
// { dg-do compile { target c++20 } }

#include "construct_at.h"

template <typename _Tp> struct __box {
  [[no_unique_address]] _Tp _M_value;
};

struct Empty {};

constexpr bool test() {
  __box<Empty> a;
  std::construct_at(&a._M_value);
  return true;
}

static_assert(test());
