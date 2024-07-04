/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_convert_int2float-2.h"

#define N 99

#define EPS 1e-8

#define TEST_LOOP(OLD_TYPE, NEW_TYPE)                                          \
  {                                                                            \
    NEW_TYPE r[N], b = 192.12;                                                 \
    OLD_TYPE a[N], pred[N];                                                    \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a[i] = (i & 1 ? i : 3 * i) * (i % 3 == 0 ? 1 : -1);                    \
	pred[i] = (i % 7 < 4);                                                 \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    test_##OLD_TYPE##_2_##NEW_TYPE (r, a, b, pred, N);                         \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	NEW_TYPE ref = pred[i] ? a[i] : b;                                     \
	if (__builtin_fabsf (r[i] - ref) > EPS)                                \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

int
main ()
{
  TEST_ALL_X2F_SAME (TEST_LOOP)
  TEST_ALL_X2F_WIDER (TEST_LOOP)
  TEST_ALL_X2F_NARROWER (TEST_LOOP)
  return 0;
}
