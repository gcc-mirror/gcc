/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define NUM_ELEMS(TYPE) ((int)(5 * (256 / sizeof (TYPE)) + 3))

#define DEF_REDUC_PLUS(TYPE)			\
  TYPE __attribute__ ((noinline, noclone))	\
  reduc_plus_##TYPE (TYPE *a, TYPE *b)		\
  {						\
    TYPE r = 0, q = 3;				\
    for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
      {						\
	r += a[i];				\
	q -= b[i];				\
      }						\
    return r * q;				\
  }

#define TEST_ALL(T) \
  T (_Float16) \
  T (float) \
  T (double)

TEST_ALL (DEF_REDUC_PLUS)

/* { dg-final { scan-assembler-times {vfredosum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 6 } } */
