// PR c++/98712
// { dg-do run { target c++20 } }

#include <compare>

struct S
{
  int s = 0;
  S(int s) : s(s) {}
  auto operator<=>(const S&) const = default;
};

struct T : S
{
  T(int s) : S(s) {}
  constexpr auto operator<=>(const T&) const = default;
};

int
main()
{
  if (T(0) >= T(1))
    __builtin_abort ();
}
