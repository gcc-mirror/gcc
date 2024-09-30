// PR c++/113063
// { dg-do link { target c++20 } }

#include <compare>

int main() {
  struct X {
    auto operator<=>(const X&) const = default;
  };
  X x;
  static_assert(noexcept(x <=> x));
  x <=> x;
  constexpr auto r = x <=> x;
}
