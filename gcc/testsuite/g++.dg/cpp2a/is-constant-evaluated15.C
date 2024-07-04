// PR c++/114580
// { dg-do compile { target c++17 } }
// { dg-options "-Wtautological-compare" }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
#if __cpp_if_consteval >= 202106L
    if consteval { return true; } else { return false; }
#else
    return __builtin_is_constant_evaluated ();
#endif
  }
}

template <typename T>
void foo ()
{
  if constexpr ((T) std::is_constant_evaluated ())	// { dg-warning "'std::is_constant_evaluated' always evaluates to true in 'if constexpr'" }
    ;							// { dg-bogus "'std::is_constant_evaluated' always evaluates to false in a non-'constexpr' function" }
}

void
bar ()
{
  foo <bool> ();
}
