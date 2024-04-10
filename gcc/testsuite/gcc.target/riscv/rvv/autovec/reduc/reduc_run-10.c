/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-signaling-nans" } */

#include <math.h>

#include "reduc-10.c"

#define NUM_ELEMS(TYPE) (73 + sizeof (TYPE))

#define INIT_VECTOR(TYPE)				\
  TYPE a[NUM_ELEMS (TYPE) + 1];				\
  for (int i = 0; i < NUM_ELEMS (TYPE) + 1; i++)	\
    {							\
      a[i] = ((i * 2) * (i & 1 ? 1 : -1) | 3);		\
      asm volatile ("" ::: "memory");			\
    }							\
    a[0] = -0.0;				        \
    a[1] = nan ("0.0");					\
    a[2] = nan ("1.0");					\
    a[3] = 0.0;						\
    a[4] = -INFINITY;					\
    a[5] = INFINITY;					\

#define TEST_REDUC_FMAXMIN(TYPE, NAME, MAXMIN_OP)		\
  {								\
    INIT_VECTOR (TYPE);						\
    TYPE r1 = reduc_##NAME##_##TYPE (a, NUM_ELEMS (TYPE));	\
    volatile TYPE r2 = -0.0;					\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)			\
      r2 = MAXMIN_OP (r2, a[i]);				\
    if (r1 != r2)						\
      __builtin_abort ();					\
  }

__attribute__ ((optimize ("1")))
int main ()
{
  TEST_FMAXMIN (TEST_REDUC_FMAXMIN)

  return 0;
}
