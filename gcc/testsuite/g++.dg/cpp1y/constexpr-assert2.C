// PR c++/65985
// { dg-do compile { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

class Angle
{
  int degrees = 0;

  constexpr auto invariant() const noexcept
  {
    return 0 <= degrees && degrees < 360;
  }

public:
  explicit constexpr Angle(int n) noexcept
    : degrees{n % 360}
  {
    assert(invariant());
  }

  /* implicit */ constexpr operator auto() const noexcept
  {
    return degrees;
  }
};

int main()
{
  static_assert(Angle{360} == 0, "");
}
