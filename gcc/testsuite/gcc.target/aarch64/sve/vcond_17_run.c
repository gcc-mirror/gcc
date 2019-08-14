/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

#include "vcond_17.c"

#define N 401

#define TEST_LOOP(CMP, EXPECT_INVALID)				\
  {								\
    __fp16 dest1[N], dest2[N], dest3[N], src[N];		\
    __fp16 a[N], b[N];						\
    for (int i = 0; i < N; ++i)					\
      {								\
	src[i] = i * i;						\
	if (i % 5 == 0)						\
	  a[i] = 0;						\
	else if (i % 3)						\
	  a[i] = i * 0.1;					\
	else							\
	  a[i] = i;						\
	if (i % 7 == 0)						\
	  b[i] = __builtin_nan ("");				\
	else if (i % 6)						\
	  b[i] = i * 0.1;					\
	else							\
	  b[i] = i;						\
	asm volatile ("" ::: "memory");				\
      }								\
    feclearexcept (FE_ALL_EXCEPT);				\
    test_##CMP##_var (dest1, src, 11, a, b, N);			\
    test_##CMP##_zero (dest2, src, 22, a, N);			\
    test_##CMP##_sel (dest3, 33, 44, a, 9, N);			\
    if (!fetestexcept (FE_INVALID) != !(EXPECT_INVALID))	\
      __builtin_abort ();					\
    for (int i = 0; i < N; ++i)					\
      {								\
	if (dest1[i] != (CMP (a[i], b[i]) ? src[i] : 11))	\
	  __builtin_abort ();					\
	if (dest2[i] != (CMP (a[i], 0) ? src[i] : 22))		\
	  __builtin_abort ();					\
	if (dest3[i] != (CMP (a[i], 9) ? 33 : 44))		\
	  __builtin_abort ();					\
      }								\
  }

int __attribute__ ((optimize (1)))
main (void)
{
  TEST_ALL (TEST_LOOP)
  return 0;
}
