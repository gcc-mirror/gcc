// { dg-do compile { target c++20 } }

#include <compare>
#include <type_traits>

struct E;
struct D {
  auto operator<=>(const D&) const = default;
  float f;
};
struct E : D {
  auto operator<=>(const E&) const = default;
  int i;
};
extern E e1, e2;
auto a = e1 <=> e2;
static_assert (std::is_same_v <decltype (a), std::partial_ordering>);
struct G;
struct H {
  constexpr auto operator<=>(const H&) const = default;
  float f;
};
struct G : H {
  constexpr auto operator<=>(const G&) const = default;
  int i;
};
extern G g1, g2;
auto c = g1 <=> g2;
static_assert (std::is_same_v <decltype (c), std::partial_ordering>);
