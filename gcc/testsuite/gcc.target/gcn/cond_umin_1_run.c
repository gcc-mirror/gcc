/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_umin_1.c"

#define N 99

#define TEST_REGREG_OPS(TYPE)                                                  \
  {                                                                            \
    TYPE x[N], y[N];                                                           \
    TYPE pred[N];                                                              \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	x[i] = i % 13;                                                         \
	y[i] = i * i;                                                          \
	pred[i] = i % 3;                                                       \
      }                                                                        \
    varith_##TYPE##_reg (x, y, pred, N);                                       \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	TYPE expected = i % 3 != 1 ? (x[i] < y[i] ? x[i] : y[i]) : 4;          \
	if (x[i] != expected)                                                  \
	  __builtin_abort ();                                                  \
	asm volatile ("" ::: "memory");                                        \
      }                                                                        \
  }

#define TEST_IMMEDIATE_OPS(VALUE, TYPE)                                        \
  {                                                                            \
    TYPE x[N], y[N];                                                           \
    TYPE pred[N];                                                              \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	x[i] = i * i;                                                          \
	pred[i] = i % 3;                                                       \
      }                                                                        \
    varithimm_##VALUE##_##TYPE (x, pred, N);                                   \
    for (int i = 0; i < N; ++i)                                                \
      {                                                                        \
	TYPE expected                                                          \
	  = i % 3 != 1 ? (x[i] < (TYPE) VALUE ? x[i] : (TYPE) VALUE) : 4;      \
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