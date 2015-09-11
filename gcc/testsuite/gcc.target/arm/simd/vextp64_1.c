/* Test the `vextp64' ARM Neon intrinsic.  */

/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-options "-save-temps -O3 -fno-inline" } */
/* { dg-add-options arm_crypto } */

#include "arm_neon.h"

extern void abort (void);

int
main (int argc, char **argv)
{
  int i;
  poly64x1_t in1 = {0};
  poly64x1_t in2 = {1};
  poly64x1_t actual = vext_p64 (in1, in2, 0);
  if (actual != in1)
    abort ();

  return 0;
}

/* Don't scan assembler for vext - it can be optimized into a move from r0. */
