/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O" } */
/* { dg-options "-O -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "sve_vcond_1.c"

#define NUM_ELEMS(X) (sizeof (X) / sizeof (X[0]))

#define TEST_VCOND_VAR(TYPE, COND, SUFFIX)		\
{							\
  TYPE x, y, a, b;					\
  for (int i = 0; i < NUM_ELEMS (x); ++i)		\
    {							\
      a[i] = i - 2;					\
      b[i] = NUM_ELEMS (x) - 2 - i;			\
      x[i] = i * 2;					\
      y[i] = -i * 3;					\
    }							\
  TYPE r = vcond_##TYPE##_##SUFFIX (x, y, a, b);	\
  for (int i = 0; i < NUM_ELEMS (x); ++i)		\
    if (r[i] != (a[i] COND b[i] ? x[i] : y[i]))		\
      __builtin_abort ();				\
}

#define TEST_VCOND_IMM(TYPE, COND, IMM, SUFFIX)		\
{							\
  TYPE x, y, a;						\
  for (int i = 0; i < NUM_ELEMS (x); ++i)		\
    {							\
      a[i] = IMM - 2 + i;				\
      x[i] = i * 2;					\
      y[i] = -i * 3;					\
    }							\
  TYPE r = vcond_imm_##TYPE##_##SUFFIX (x, y, a);	\
  for (int i = 0; i < NUM_ELEMS (x); ++i)		\
    if (r[i] != (a[i] COND IMM ? x[i] : y[i]))		\
      __builtin_abort ();				\
}


int main (int argc, char **argv)
{
  TEST_VAR_ALL (TEST_VCOND_VAR)
  TEST_IMM_ALL (TEST_VCOND_IMM)
  return 0;
}
