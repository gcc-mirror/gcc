/* Test C23 Checked Integer Arithmetic macros in <stdckdint.h>.  */
/* { dg-do run } */
/* { dg-options "-std=c23" } */

#include <stdckdint.h>

#if __STDC_VERSION_STDCKDINT_H__ != 202311L
# error __STDC_VERSION_STDCKDINT_H__ not defined to 202311L
#endif

extern void abort (void);

int
main ()
{
  unsigned int a;
  if (ckd_add (&a, 1, 2) || a != 3)
    abort ();
  if (ckd_add (&a, ~2U, 2) || a != ~0U)
    abort ();
  if (!ckd_add (&a, ~2U, 4) || a != 1)
    abort ();
  if (ckd_sub (&a, 42, 2) || a != 40)
    abort ();
  if (!ckd_sub (&a, 11, ~0ULL) || a != 12)
    abort ();
  if (ckd_mul (&a, 42, 16U) || a != 672)
    abort ();
  if (ckd_mul (&a, ~0UL, 0) || a != 0)
    abort ();
  if (ckd_mul (&a, 1, ~0U) || a != ~0U)
    abort ();
  if (ckd_mul (&a, ~0UL, 1) != (~0UL > ~0U) || a != ~0U)
    abort ();
  static_assert (_Generic (ckd_add (&a, 1, 1), bool: 1, default: 0));
  static_assert (_Generic (ckd_sub (&a, 1, 1), bool: 1, default: 0));
  static_assert (_Generic (ckd_mul (&a, 1, 1), bool: 1, default: 0));
  signed char b;
  if (ckd_add (&b, 8, 12) || b != 20)
    abort ();
  if (ckd_sub (&b, 8UL, 12ULL) || b != -4)
    abort ();
  if (ckd_mul (&b, 2, 3) || b != 6)
    abort ();
  unsigned char c;
  if (ckd_add (&c, 8, 12) || c != 20)
    abort ();
  if (ckd_sub (&c, 8UL, 12ULL) != (-4ULL > (unsigned char) -4U)
      || c != (unsigned char) -4U)
    abort ();
  if (ckd_mul (&c, 2, 3) || c != 6)
    abort ();
  long long d;
  if (ckd_add (&d, ~0U, ~0U) != (~0U + 1ULL < ~0U)
      || d != (long long) (2 * (unsigned long long) ~0U))
    abort ();
  if (ckd_sub (&d, 0, 0) || d != 0)
    abort ();
  if (ckd_mul (&d, 16, 1) || d != 16)
    abort ();
}
