/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (128 / sizeof (TYPE))

#define DUP_FN(TYPE)				\
void __attribute__ ((noinline, noclone))	\
dup_##TYPE (TYPE *r, TYPE v)			\
{						\
  for (int i = 0; i < NUM_ELEMS (TYPE); i++)	\
    r[i] = v;					\
}

DUP_FN (int8_t)
DUP_FN (int16_t)
DUP_FN (int32_t)
DUP_FN (int64_t)
DUP_FN (_Float16)
DUP_FN (float)
DUP_FN (double)

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, w[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, w[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, w[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, x[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, h[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, s[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, d[0-9]+\n} 1 } } */
