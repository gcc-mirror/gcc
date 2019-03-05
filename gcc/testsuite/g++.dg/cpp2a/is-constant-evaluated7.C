// P0595R2
// PR c++/88977
// { dg-do compile { target c++11 } }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

template<bool B> constexpr bool foo () { return B; }

constexpr bool x = foo<std::is_constant_evaluated ()> ();
constexpr bool y = foo<__builtin_is_constant_evaluated ()> ();
static_assert (x, "");
static_assert (y, "");
