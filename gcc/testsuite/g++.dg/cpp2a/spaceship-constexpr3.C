// PR c++/96497
// { dg-do compile { target c++20 } }

#include <compare>

static_assert(std::partial_ordering(std::strong_ordering::less) < 0);
static_assert(std::partial_ordering(1 <=> 2) < 0);
