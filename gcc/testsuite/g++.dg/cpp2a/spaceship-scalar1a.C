// { dg-do run { target c++20 } }

#include <compare>

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

template <class T, class U, class R>
constexpr bool check(T a, U b, R expected)
{
  auto r = a <=> b;
  static_assert (__is_same_as (decltype (r), R));
  return r == expected;
}

int main()
{
  static_assert (check (1, 2, std::strong_ordering::less));

  enum E1 { a = 0 };
  static_assert (check (a, 1, std::strong_ordering::less));

  enum class E2 { a, b };
  static_assert (check (E2::a, E2::b, std::strong_ordering::less));

  int ar[2];
  static_assert (check (&ar[1], &ar[0], std::strong_ordering::greater));

  static_assert (check (3.14, 3.14, std::partial_ordering::equivalent));
}
