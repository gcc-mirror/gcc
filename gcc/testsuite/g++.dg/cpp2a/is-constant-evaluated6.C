// P0595R2
// { dg-do compile { target c++14 } }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

int a;

constexpr bool
foo (int x)
{
  return __builtin_constant_p (x);
}

constexpr bool
bar (int x)
{
  return __builtin_constant_p (x + a);
}

static_assert (__builtin_constant_p (0) + 2 * std::is_constant_evaluated () == 3, "");
static_assert (__builtin_constant_p (a) + 2 * std::is_constant_evaluated () == 2, "");
static_assert (foo (0) + 2 * std::is_constant_evaluated () == 3, "");
static_assert (bar (0) + 2 * std::is_constant_evaluated () == 2, "");
