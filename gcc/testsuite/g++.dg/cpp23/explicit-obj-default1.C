// P0847R7
// { dg-do run { target c++23 } }

// defaulted comparison operators

#include <compare>

struct S {
  int _v;
  bool operator==(this S const&, S const&) = default;
  auto operator<=>(this S const&, S const&) = default;
};

int main()
{
  S const a_10{10};
  S const b_10{10};
  S const c_20{20};
  S const d_5{5};

  if (a_10 != b_10)
    __builtin_abort ();
  if (c_20 == a_10)
    __builtin_abort ();
  if (!(a_10 == b_10))
    __builtin_abort ();
  if (!(c_20 != a_10))
    __builtin_abort ();

  if (a_10 < b_10)
    __builtin_abort ();
  if (a_10 > b_10)
    __builtin_abort ();
  if (!(a_10 <= b_10))
    __builtin_abort ();
  if (!(a_10 >= b_10))
    __builtin_abort ();

  if (!(a_10 < c_20))
    __builtin_abort ();
  if (a_10 > c_20)
    __builtin_abort ();
  if (!(a_10 <= c_20))
    __builtin_abort ();
  if (a_10 >= c_20)
    __builtin_abort ();

  if (a_10 < d_5)
    __builtin_abort ();
  if (!(a_10 > d_5))
    __builtin_abort ();
  if (a_10 <= d_5)
    __builtin_abort ();
  if (!(a_10 >= d_5))
    __builtin_abort ();
}

