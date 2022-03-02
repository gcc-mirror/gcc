// PR c++/96242
// { dg-do compile { target c++20 } }

#include <compare>

template<bool B>
struct X {
  auto operator<=>(const X&) const noexcept(B) = default;
  bool operator==(const X&) const noexcept(!B) = default;
};

X<true> x_t;
static_assert(noexcept(x_t <=> x_t));
static_assert(noexcept(x_t < x_t));
static_assert(!noexcept(x_t == x_t));
static_assert(!noexcept(x_t != x_t));

X<false> x_f;
static_assert(!noexcept(x_f <=> x_f));
static_assert(!noexcept(x_f < x_f));
static_assert(noexcept(x_f == x_f));
static_assert(noexcept(x_f != x_f));
