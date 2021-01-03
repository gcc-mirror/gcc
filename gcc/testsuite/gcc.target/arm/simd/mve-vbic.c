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
FUNC(s, int, 32, 2, & ~ , vbic)
FUNC(u, uint, 32, 2, & ~ , vbic)
FUNC(s, int, 16, 4, & ~ , vbic)
FUNC(u, uint, 16, 4, & ~ , vbic)
FUNC(s, int, 8, 8, & ~ , vbic)
FUNC(u, uint, 8, 8, & ~ , vbic)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, & ~ , vbic)
FUNC(u, uint, 32, 4, & ~ , vbic)
FUNC(s, int, 16, 8, & ~ , vbic)
FUNC(u, uint, 16, 8, & ~ , vbic)
FUNC(s, int, 8, 16, & ~ , vbic)
FUNC(u, uint, 8, 16, & ~ , vbic)

/* 64-bit vectors.  */
FUNC_IMM(s, int, 32, 2, & ~, vbicimm)
FUNC_IMM(u, uint, 32, 2, & ~, vbicimm)
FUNC_IMM(s, int, 16, 4, & ~, vbicimm)
FUNC_IMM(u, uint, 16, 4, & ~, vbicimm)
FUNC_IMM(s, int, 8, 8, & ~, vbicimm)
FUNC_IMM(u, uint, 8, 8, & ~, vbicimm)

/* 128-bit vectors.  */
FUNC_IMM(s, int, 32, 4, & ~, vbicimm)
FUNC_IMM(u, uint, 32, 4, & ~, vbicimm)
FUNC_IMM(s, int, 16, 8, & ~, vbicimm)
FUNC_IMM(u, uint, 16, 8, & ~, vbicimm)
FUNC_IMM(s, int, 8, 16, & ~, vbicimm)
FUNC_IMM(u, uint, 8, 16, & ~, vbicimm)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* We emit vand.i[16|32] qX, #XX for the first four versions of the
   128-bit vector vbicimm tests.  */
/* For some reason, we do not generate the immediate version for
   int8x16 and uint8x16, we still use vldr to load the vector of
   immediates.  */
/* { dg-final { scan-assembler-times {vbic\tq[0-9]+, q[0-9]+, q[0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {vand.i[0-9]+\tq[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {vand\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */
