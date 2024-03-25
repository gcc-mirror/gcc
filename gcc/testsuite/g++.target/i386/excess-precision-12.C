// Excess precision tests.  Test implicit conversions in 3-way comparisons:
// excess precision in C++.
// { dg-do run { target c++20 } }
// { dg-options "-mfpmath=387 -fexcess-precision=standard" }

#include <compare>
#include <cstdlib>

int
main (void)
{
  float f = 0x1p63f;
  unsigned long long int u = (1ULL << 63) + 1;

  if ((f <=> u) >= 0)
    std::abort ();

  if ((u <=> f) <= 0)
    std::abort ();
}
