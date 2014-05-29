/* Test the `vextQp64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-options "-save-temps -O3 -fno-inline" } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

extern void abort (void);

poly64x2_t
test_vextq_p64_1 (poly64x2_t a, poly64x2_t b)
{
  return vextq_p64(a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  poly64x2_t in1 = {0, 1};
  poly64x2_t in2 = {2, 3};
  poly64x2_t actual = test_vextq_p64_1 (in1, in2);
  for (i = 0; i < 2; i++)
    if (actual[i] != i + 1)
      abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "vext\.64\[ \t\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #\[0-9\]+!?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
