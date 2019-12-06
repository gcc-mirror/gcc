/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -fno-vect-cost-model" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (32 / sizeof (TYPE))

#define REDUC_PTR(DSTTYPE, SRCTYPE)				\
void reduc_ptr_##DSTTYPE##_##SRCTYPE (DSTTYPE *restrict sum,	\
				      SRCTYPE *restrict array,	\
				      int count)		\
{								\
  *sum = 0;							\
  for (int i = 0; i < count; ++i)				\
    *sum += array[i];						\
}

/* Widening reductions.  */
REDUC_PTR (int32_t, int8_t)
REDUC_PTR (int32_t, int16_t)

REDUC_PTR (int64_t, int8_t)
REDUC_PTR (int64_t, int16_t)
REDUC_PTR (int64_t, int32_t)

REDUC_PTR (float, _Float16)
REDUC_PTR (double, float)

/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfaddv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfaddv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
