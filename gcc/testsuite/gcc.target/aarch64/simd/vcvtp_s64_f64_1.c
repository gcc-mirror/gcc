/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort ();

int
main()
{
  volatile float64x1_t a = {0.5};
  int64x1_t b1 = vcvtp_s64_f64 (a);

  if (b1[0] != 1)
    abort ();

  volatile float64x1_t a2 = {-0.5};
  int64x1_t b2 = vcvtp_s64_f64 (a2);

  if (b2[0] != 0)
    abort ();

  return 0;
}
/* { dg-final { scan-assembler "fcvtps\[ \t\]+\[xX\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" } } */
