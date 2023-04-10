/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "smin_1.c"

#define N 99

#define TEST_REGREG_OPS(TYPE)                                                  \
  {                                                                            \
    TYPE x[N], y[N];                                                           \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	x[i] = i % 13;                                                         \
	y[i] = i * i;                                                          \
      }                                                                        \
    varith_##TYPE##_reg (x, y, N);                                             \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	TYPE expected = x[i] < y[i] ? x[i] : y[i];                             \
	if (x[i] != expected)                                                  \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

#define TEST_IMMEDIATE_OPS(VALUE, TYPE, NAME)                                  \
  {                                                                            \
    TYPE x[N], y[N];                                                           \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	x[i] = i * i;                                                          \
      }                                                                        \
    varithimm_##NAME##_##TYPE (x, N);                                          \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	TYPE expected = x[i] < (TYPE) VALUE ? x[i] : (TYPE) VALUE;             \
	if (x[i] != expected)                                                  \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

int
main (void)
{
  TEST_ALL (TEST_REGREG_OPS, TEST_IMMEDIATE_OPS)
  return 0;
}