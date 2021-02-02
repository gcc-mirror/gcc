// { dg-do run { target c++20 } }
// { dg-options "" }

#include <compare>

struct C {
  int y;
  int x[4];
  auto operator<=>(C const&) const = default;
};

struct D {
  int y;
  int x[1];
  auto operator<=>(D const&) const = default;
};

struct E {
  int y;
  int x[0];
  auto operator<=>(E const&) const = default;
};

int
main ()
{
  constexpr C c1 = { 1, { 2, 3, 4, 5 } };
  constexpr C c2 = { 1, { 2, 3, 5, 4 } };
  constexpr C c3 = { 1, { 2, 2, 6, 7 } };
  static_assert (c1 < c2);
  static_assert (c3 < c1);
  constexpr D d1 = { 1, { 2 } };
  constexpr D d2 = { 1, { 3 } };
  constexpr D d3 = { 1, { 1 } };
  static_assert (d1 < d2);
  static_assert (d3 < d1);
  constexpr E e1 = { 1, {} };
  constexpr E e2 = { 2, {} };
  constexpr E e3 = { 1, {} };
  static_assert (e1 < e2);
  static_assert (e1 == e3);
  C c4 = { 1, { 2, 3, 4, 5 } };
  C c5 = { 1, { 2, 3, 5, 4 } };
  C c6 = { 1, { 2, 2, 6, 7 } };
  if (c4 >= c5 || c6 >= c4)
    __builtin_abort ();
  D d4 = { 1, { 2 } };
  D d5 = { 1, { 3 } };
  D d6 = { 1, { 1 } };
  if (d4 >= d5 || d6 >= d4)
    __builtin_abort ();
  E e4 = { 1, {} };
  E e5 = { 2, {} };
  E e6 = { 1, {} };
  if (e4 >= e5 || e4 != e6)
    __builtin_abort ();
}
