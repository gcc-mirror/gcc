/* { dg-do compile { target powerpc-*-linux*paired* } } */
/* { dg-options "-mpaired -m32 -ffinite-math-only" } */

/* Test PowerPC PAIRED extensions.  */

#include <paired.h>

vector float a, b, c, d;

void
test_api ()
{
  b = paired_msub (b, c, d);
  b = paired_madd (b, c, d);
  b = paired_nmadd (b, c, d);
  b = paired_nmsub (b, c, d);
  b = paired_sum0 (a, b, c);
  b = paired_sum1 (a, b, c);
  b = paired_div (b, c);
  b = paired_add (a, c);
  b = paired_sub (a, c);
  b = paired_mul (a, c);
  b = paired_neg (a);
  b = paired_muls0 (a, c);
  b = paired_muls1 (a, c);
  b = paired_madds0 (a, c, d);
  b = paired_madds1 (a, c, d);
  b = paired_merge00 (a, c);
  b = paired_merge01 (a, c);
  b = paired_merge10 (a, c);
  b = paired_merge11 (a, c);
  b = paired_abs (a);
  b = paired_nabs (a);
  b = paired_sqrt (a);
  b = paired_res (a);
  b = paired_sel (a, b, c);
}

int
main (void)
{
  test_api ();
  return 0;
}

