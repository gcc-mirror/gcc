/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O2 -Wvla-larger-than=40" } */

#include <stdint.h>

void f0 (void *);
void
f1 (__SIZE_TYPE__ a)
{
  if (a <= 10)
    {
      // 10 * 4 bytes = 40: OK!
      uint32_t x[a];
      f0 (x);
    }
}

void
f2 (__SIZE_TYPE__ a)
{
  if (a <= 11)
    {
      // 11 * 4 bytes = 44: Not OK.
      uint32_t x[a]; // { dg-warning "array may be too large" }
      // { dg-message "note:.*argument may be as large as 44" "note" { target *-*-* } .-1 }
      f0 (x);
    }
}

void
f3 (__SIZE_TYPE__ a, __SIZE_TYPE__ b)
{
  if (a <= 5 && b <= 3)
    {
      // 5 * 3 * 4 bytes = 60: Not OK.
      uint32_t x[a][b]; // { dg-warning "array may be too large" }
      f0 (x);
    }
}

void
f4 (__SIZE_TYPE__ a, __SIZE_TYPE__ b)
{
  if (a <= 5 && b <= 2)
    {
      // 5 * 2 * 4 bytes = 40 bytes: OK!
      uint32_t x[a][b];
      f0 (x);
    }
}

void
f5 (__SIZE_TYPE__ len)
{
  // Test that a direct call to __builtin_alloca_with_align is not
  // confused with a VLA.
  void *p = __builtin_alloca_with_align (len, 8);
  f0 (p);
}

void
f6 (unsigned stuff)
{
  int n = 7000;
  do {
    char a[n]; // { dg-warning "variable-length array is too large" }
    f0 (a);
  } while (stuff--);
}
