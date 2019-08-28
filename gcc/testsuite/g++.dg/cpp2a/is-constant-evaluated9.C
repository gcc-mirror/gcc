// PR c++/91428 - warn about std::is_constant_evaluated in if constexpr.
// { dg-do compile { target c++2a } }
// { dg-options "-Wtautological-compare" }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated (); 
  }
}

constexpr int
foo(int i)
{
  if constexpr (std::is_constant_evaluated ()) // { dg-warning ".std::is_constant_evaluated. always evaluates to true in .if constexpr." }
    return 42;
  else
    return i;
}

constexpr int
foo2(int i)
{
  if constexpr (__builtin_is_constant_evaluated ()) // { dg-warning ".std::is_constant_evaluated. always evaluates to true in .if constexpr." }
    return 42;
  else
    return i;
}

constexpr int
foo3(int i)
{
  // I is not a constant expression but we short-circuit it.
  if constexpr (__builtin_is_constant_evaluated () || i)
    return 42;
  else
    return i;
}

constexpr int
foo4(int i)
{
  const int j = 0;
  if constexpr (j && __builtin_is_constant_evaluated ())
    return 42;
  else
    return i;
}
