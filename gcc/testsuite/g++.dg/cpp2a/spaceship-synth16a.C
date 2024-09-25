// PR c++/116289
// PR c++/113063
// { dg-do link { target c++20 } }
// A version of spaceship-synth16.C where the local class isn't empty.

#include <compare>

int main() {
  struct X {
    int m = 0;
    auto operator<=>(const X&) const = default;
  };
  X x;
  static_assert(noexcept(x <=> x));
  x <=> x;
  constexpr auto r = X{} <=> X{};
}
