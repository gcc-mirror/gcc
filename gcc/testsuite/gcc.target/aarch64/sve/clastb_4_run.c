/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline" } */

#include "clastb_4.c"

extern void abort (void) __attribute__ ((noreturn));

int __attribute__ ((optimize (1)))
main (void)
{
  TYPE a[N] = {
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
  31, 32
  };
  __builtin_memset (a+32, 43, (N-32)*sizeof (TYPE));

  TYPE ret = condition_reduction (a, 16);

  if (ret != 10)
    abort ();

  return 0;
}
