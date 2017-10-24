/* Test AAPCS layout (alignment) for callee.  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O -fno-inline" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

extern int memcmp (const void *s1, const void *s2, __SIZE_TYPE__ n);
extern void abort (void);

typedef __attribute__((aligned (4))) int32x4_t unalignedvec;

unalignedvec a = {11, 13};
unalignedvec b = {17, 19};

void
foo (int r0, unalignedvec r2, int s0, unalignedvec s8)
{
  if (r0 != 2 || s0 != 6
      || memcmp ( (void *) &r2, (void *) &a, 16)
      || memcmp ( (void *) &s8, (void *) &b, 16))
    abort ();
}

int
main (int argc, char **argv)
{
  foo (2, a, 6, b);
  return 0;
}
