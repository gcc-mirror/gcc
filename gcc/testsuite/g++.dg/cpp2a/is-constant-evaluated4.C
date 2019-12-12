// P0595R2
// { dg-do compile { target c++14 } }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

constexpr int
foo () noexcept
{
  return std::is_constant_evaluated () ? 5 : 12;
}

static_assert (std::is_constant_evaluated (), "");
static_assert (foo () == 5, "");
