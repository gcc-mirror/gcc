/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort ();

int
main()
{
  volatile float64x1_t a = {0.5};
  uint64x1_t b1 = vcvtp_u64_f64 (a);

  if (b1[0] != 1)
    abort ();

  return 0;
}
/* { dg-final { scan-assembler "fcvtpu\[ \t\]+\[xX\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" } } */
