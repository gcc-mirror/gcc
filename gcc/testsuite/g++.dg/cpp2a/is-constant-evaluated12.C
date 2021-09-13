// PR c++/100995
// { dg-do compile { target c++20 } }
// { dg-options "-Wtautological-compare" }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

template <int I = []() constexpr { if (std::is_constant_evaluated()) return 1; return 0; }()>
struct X { };

template <int I = [](){ if (std::is_constant_evaluated()) return 1; return 0; }()>
struct Y { };

template <int I = [](){ if constexpr (std::is_constant_evaluated()) return 1; return 0; }()> // { dg-warning "always evaluates to true" }
struct Z { };

constexpr bool b = true;

#define __glibcxx_assert(cond) \
  if (__builtin_is_constant_evaluated() && !bool(cond)) \
__builtin_unreachable()
#define CHECK __builtin_is_constant_evaluated() // { dg-warning "always evaluates to false" }
#define CHECK2 __builtin_is_constant_evaluated()

int
foo ()
{
  if (std::is_constant_evaluated ()) // { dg-warning "always evaluates to false" }
    return 1;
  __glibcxx_assert(b);
  if (CHECK && b)
    return 2;
  if (CHECK2)
    return 3;
  return 0;
}

constexpr int
bar ()
{
  if (std::is_constant_evaluated ())
    return 1;
  if constexpr (std::is_constant_evaluated ()) // { dg-warning "always evaluates to true" }
    return 2;
  if constexpr (std::is_constant_evaluated () && b) // { dg-warning "always evaluates to true" }
    return 3;
  if constexpr (!std::is_constant_evaluated ()) // { dg-warning "always evaluates to true" }
    return 4;
  return 0;
}

consteval int
baz ()
{
  if (std::is_constant_evaluated ()) // { dg-warning "always evaluates to true" }
    return 1;
  return 0;
}

int
qux ()
{
  if (({ static bool a = std::is_constant_evaluated (); a; }))
    return 1;
  if (({ bool a = std::is_constant_evaluated (); a; }))
    return 2;
  if (static bool a = std::is_constant_evaluated (); a)
    return 3;
  if (bool a = std::is_constant_evaluated (); a)
    return 4;
  if constexpr (constexpr bool a = std::is_constant_evaluated (); a)
    return 5;
  return 0;
}
