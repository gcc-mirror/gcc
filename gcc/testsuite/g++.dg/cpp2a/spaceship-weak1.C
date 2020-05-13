// Test explicit weak_ordering.
// { dg-do compile { target c++20 } }

#include <compare>
struct A
{
  int i;
  std::weak_ordering operator<=> (const A&) const = default;
};

constexpr A a = { 42 };
constexpr auto c = a <=> a;
static_assert (std::same_as <decltype (c), const std::weak_ordering>);
static_assert (std::is_eq (c));

