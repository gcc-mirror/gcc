// { dg-do compile { target c++2a } }
// { dg-options "-Wtautological-compare" }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

template<typename>
constexpr int
foo(int i)
{
  if constexpr (std::is_constant_evaluated ()) // { dg-warning ".std::is_constant_evaluated. always evaluates to true in .if constexpr." }
    return 42;
  else
    return i;
}

template<typename>
constexpr int
foo2(int i)
{
  if constexpr (__builtin_is_constant_evaluated ()) // { dg-warning ".std::is_constant_evaluated. always evaluates to true in .if constexpr." }
    return 42;
  else
    return i;
}
