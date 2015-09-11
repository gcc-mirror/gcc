/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

#include "arm_neon.h"

extern void abort (void);

int
main (int argc, char **argv)
{
  uint64_t got;
  uint64_t exp = UINT64_C (0x0001000100003b9b);
  int16x4_t val1 = vcreate_s16 (0x7fff800080007ffful);
  int16x4_t val2 = vcreate_s16 (0x80007fff80004464ul);
  int16x4_t result;
  /* Avoid folding away the sub early.  */
  asm volatile ("mov %d0, %0.d[0]":"+w"(val1));

  /* Expect "result" = 0001000100003b9b.  */
  result = vabs_s16 (vsub_s16 (val1, val2));

  got = vget_lane_u64 (vreinterpret_u64_s16 (result), 0);
  if (exp != got)
    abort ();

  return 0;
}


/* { dg-final { scan-assembler-not "sabd" } } */
