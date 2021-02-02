/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define FUNC(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, TYPE##BITS##_t *a, TYPE##BITS##_t *b) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = a[i] OP b[i];						\
    }									\
}

#define FUNC_IMM(SIGN, TYPE, BITS, NB, OP, NAME)				\
  void test_ ## NAME ##_ ## SIGN ## BITS ## x ## NB (TYPE##BITS##_t * __restrict__ dest, TYPE##BITS##_t *a) { \
    int i;								\
    for (i=0; i<NB; i++) {						\
      dest[i] = a[i] OP 1;						\
    }									\
}

/* 64-bit vectors.  */
FUNC(s, int, 32, 2, ^, veor)
FUNC(u, uint, 32, 2, ^, veor)
FUNC(s, int, 16, 4, ^, veor)
FUNC(u, uint, 16, 4, ^, veor)
FUNC(s, int, 8, 8, ^, veor)
FUNC(u, uint, 8, 8, ^, veor)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, ^, veor)
FUNC(u, uint, 32, 4, ^, veor)
FUNC(s, int, 16, 8, ^, veor)
FUNC(u, uint, 16, 8, ^, veor)
FUNC(s, int, 8, 16, ^, veor)
FUNC(u, uint, 8, 16, ^, veor)

/* 64-bit vectors.  */
FUNC_IMM(s, int, 32, 2, ^, veorimm)
FUNC_IMM(u, uint, 32, 2, ^, veorimm)
FUNC_IMM(s, int, 16, 4, ^, veorimm)
FUNC_IMM(u, uint, 16, 4, ^, veorimm)
FUNC_IMM(s, int, 8, 8, ^, veorimm)
FUNC_IMM(u, uint, 8, 8, ^, veorimm)

/* 128-bit vectors.  */
FUNC_IMM(s, int, 32, 4, ^, veorimm)
FUNC_IMM(u, uint, 32, 4, ^, veorimm)
FUNC_IMM(s, int, 16, 8, ^, veorimm)
FUNC_IMM(u, uint, 16, 8, ^, veorimm)
FUNC_IMM(s, int, 8, 16, ^, veorimm)
FUNC_IMM(u, uint, 8, 16, ^, veorimm)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* Although float16 and float32 types are supported at assembly level,
   we cannot test them with the '^' operator, so we check only the
   integer variants.  */
/* { dg-final { scan-assembler-times {veor\tq[0-9]+, q[0-9]+, q[0-9]+} 12 } } */
