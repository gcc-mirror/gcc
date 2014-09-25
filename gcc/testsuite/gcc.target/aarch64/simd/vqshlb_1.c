/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

extern void abort ();

int
main (int argc, char **argv)
{
  int8_t arg1 = -1;
  int8_t arg2 = 127;
  int8_t exp = -128;
  int8_t got = vqshlb_s8 (arg1, arg2);

  if (exp != got)
    abort ();

  return 0;
}

