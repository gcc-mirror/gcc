/* { dg-do run { target { aarch64_sve_hw } } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include "reduc_1.c"

#define NUM_ELEMS(TYPE) (73 + sizeof (TYPE))

#define INIT_VECTOR(TYPE)				\
  TYPE a[NUM_ELEMS (TYPE) + 1];				\
  for (int i = 0; i < NUM_ELEMS (TYPE) + 1; i++)	\
    {							\
      a[i] = ((i * 2) * (i & 1 ? 1 : -1) | 3);		\
      asm volatile ("" ::: "memory");			\
    }

#define TEST_REDUC_PLUS(TYPE)				\
  {							\
    INIT_VECTOR (TYPE);					\
    TYPE r1 = reduc_plus_##TYPE (a, NUM_ELEMS (TYPE));	\
    volatile TYPE r2 = 0;				\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)		\
      r2 += a[i];					\
    if (r1 != r2)					\
      __builtin_abort ();				\
  }

#define TEST_REDUC_MAXMIN(TYPE, NAME, CMP_OP)			\
  {								\
    INIT_VECTOR (TYPE);						\
    TYPE r1 = reduc_##NAME##_##TYPE (a, NUM_ELEMS (TYPE));	\
    volatile TYPE r2 = 13;					\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)			\
      r2 = a[i] CMP_OP r2 ? a[i] : r2;				\
    if (r1 != r2)						\
      __builtin_abort ();					\
  }

#define TEST_REDUC_BITWISE(TYPE, NAME, BIT_OP)			\
  {								\
    INIT_VECTOR (TYPE);						\
    TYPE r1 = reduc_##NAME##_##TYPE (a, NUM_ELEMS (TYPE));	\
    volatile TYPE r2 = 13;					\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)			\
      r2 BIT_OP a[i];						\
    if (r1 != r2)						\
      __builtin_abort ();					\
  }

int main ()
{
  TEST_PLUS (TEST_REDUC_PLUS)
  TEST_MAXMIN (TEST_REDUC_MAXMIN)
  TEST_BITWISE (TEST_REDUC_BITWISE)

  return 0;
}
