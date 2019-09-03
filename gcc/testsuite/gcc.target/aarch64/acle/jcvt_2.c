/* Test the __jcvt ACLE intrinsic.  */
/* { dg-do run } */
/* { dg-options "-O2 -march=armv8.3-a -save-temps" } */
/* { dg-require-effective-target aarch64_fjcvtzs_hw } */

#include <arm_acle.h>

extern void abort (void);

#ifdef __ARM_FEATURE_JCVT
volatile int32_t x;

int __attribute__((noinline))
foo (double a, int b, int c)
{
  b = b > c;
  x = __jcvt (a);
  return b;
}

int
main (void)
{
  int x = foo (1.1, 2, 3);
  if (x)
    abort ();

  return 0;
}

#endif

/* { dg-final { scan-assembler-times "fjcvtzs\tw\[0-9\]+, d\[0-9\]+\n" 1 } } */
