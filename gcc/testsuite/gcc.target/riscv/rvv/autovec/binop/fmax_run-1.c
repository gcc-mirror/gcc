/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-signaling-nans" } */

#include <math.h>
#include "fmax-1.c"

#define N 99

#define TEST_LOOP(FN, SUFFIX, TYPE)                                            \
  {                                                                            \
    TYPE dst[N], x[N], y[N];                                                   \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	x[i] = i;                                                              \
	dst[i] = i;                                                            \
	y[i] = ((i & 1) - 1) * i * i;                                          \
      }                                                                        \
    y[0] = -0.0;                                                               \
    y[1] = 0.0;                                                                \
    y[2] = nan ("0.0");                                                        \
    y[3] = INFINITY;                                                           \
    y[4] = -INFINITY;                                                          \
    x[5] = -0.0;                                                               \
    x[6] = 0.0;                                                                \
    x[7] = nan ("0.0");                                                        \
    x[8] = INFINITY;                                                           \
    x[9] = -INFINITY;                                                          \
    dst[5] = -0.0;                                                             \
    dst[6] = 0.0;                                                              \
    dst[7] = nan ("0.0");                                                      \
    dst[8] = INFINITY;                                                         \
    dst[9] = -INFINITY;                                                        \
    test_##TYPE (dst, y, N);                                                   \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	double ref = FN (SUFFIX) (x[i], y[i]);                                 \
	if (dst[i] != ref)                                                     \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

int __attribute__ ((optimize ("1"))) main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
