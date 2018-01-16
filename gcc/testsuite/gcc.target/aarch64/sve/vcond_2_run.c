/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "vcond_2.c"

#define N 97

#define TEST_VCOND_VAR(DATA_TYPE, CMP_TYPE, COND, SUFFIX)	\
{								\
  DATA_TYPE x[N], y[N], r[N];					\
  CMP_TYPE a[N], b[N];						\
  for (int i = 0; i < N; ++i)					\
    {								\
      x[i] = i;							\
      y[i] = (i & 1) + 5;					\
      a[i] = i - N / 3;						\
      b[i] = N - N / 3 - i;					\
      asm volatile ("" ::: "memory");				\
    }								\
  vcond_var_##CMP_TYPE##_##SUFFIX (r, x, y, a, b, N);		\
  for (int i = 0; i < N; ++i)					\
    if (r[i] != (a[i] COND b[i] ? x[i] : y[i]))			\
      __builtin_abort ();					\
}

#define TEST_VCOND_IMM(DATA_TYPE, CMP_TYPE, COND, IMM, SUFFIX)	\
{								\
  DATA_TYPE x[N], y[N], r[N];					\
  CMP_TYPE a[N];						\
  for (int i = 0; i < N; ++i)					\
    {								\
      x[i] = i;							\
      y[i] = (i & 1) + 5;					\
      a[i] = IMM - N / 3 + i;					\
      asm volatile ("" ::: "memory");				\
    }								\
  vcond_imm_##CMP_TYPE##_##SUFFIX (r, x, y, a, N);		\
  for (int i = 0; i < N; ++i)					\
    if (r[i] != (a[i] COND (CMP_TYPE) IMM ? x[i] : y[i]))	\
      __builtin_abort ();					\
}

int __attribute__ ((optimize (1)))
main (int argc, char **argv)
{
  TEST_VAR_ALL (TEST_VCOND_VAR)
  TEST_IMM_ALL (TEST_VCOND_IMM)
  return 0;
}
