/* { dg-do run { target { riscv_zvfh && riscv_zfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "cond_convert_float2int_zvfh-1.h"

#define N 77

#define TEST_LOOP(OLD_TYPE, NEW_TYPE)                                          \
  {                                                                            \
    NEW_TYPE r[N], b[N];                                                       \
    OLD_TYPE a[N], pred[N];                                                    \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	a[i] = (i & 1 ? i : 1.1 * i) * (i % 3 == 0 ? 1.2 : -1.5);              \
	b[i] = (i % 9) * (i % 7 + 1);                                          \
	pred[i] = (i % 7 < 4);                                                 \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
    test_##OLD_TYPE##_2_##NEW_TYPE (r, a, b, pred, N);                         \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	NEW_TYPE ref = pred[i] ? a[i] : b[i];                                  \
	if (r[i] != ref)                                                       \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

int
main ()
{
  TEST_ALL_F2X_SAME (TEST_LOOP)
  TEST_ALL_F2X_WIDER (TEST_LOOP)
  TEST_ALL_F2X_NARROWER (TEST_LOOP)
  return 0;
}
