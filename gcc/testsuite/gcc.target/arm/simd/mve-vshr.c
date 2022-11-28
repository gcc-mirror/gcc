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
      dest[i] = a[i] OP 5;						\
    }									\
}

/* 64-bit vectors.  */
FUNC(s, int, 32, 2, >>, vshr)
FUNC(u, uint, 32, 2, >>, vshr)
FUNC(s, int, 16, 4, >>, vshr)
FUNC(u, uint, 16, 4, >>, vshr)
FUNC(s, int, 8, 8, >>, vshr)
FUNC(u, uint, 8, 8, >>, vshr)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, >>, vshr)
FUNC(u, uint, 32, 4, >>, vshr)
FUNC(s, int, 16, 8, >>, vshr)
FUNC(u, uint, 16, 8, >>, vshr)
FUNC(s, int, 8, 16, >>, vshr)
FUNC(u, uint, 8, 16, >>, vshr)

/* 64-bit vectors.  */
FUNC_IMM(s, int, 32, 2, >>, vshrimm)
FUNC_IMM(u, uint, 32, 2, >>, vshrimm)
FUNC_IMM(s, int, 16, 4, >>, vshrimm)
FUNC_IMM(u, uint, 16, 4, >>, vshrimm)
FUNC_IMM(s, int, 8, 8, >>, vshrimm)
FUNC_IMM(u, uint, 8, 8, >>, vshrimm)

/* 128-bit vectors.  */
FUNC_IMM(s, int, 32, 4, >>, vshrimm)
FUNC_IMM(u, uint, 32, 4, >>, vshrimm)
FUNC_IMM(s, int, 16, 8, >>, vshrimm)
FUNC_IMM(u, uint, 16, 8, >>, vshrimm)
FUNC_IMM(s, int, 8, 16, >>, vshrimm)
FUNC_IMM(u, uint, 8, 16, >>, vshrimm)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* Vector right shifts use vneg and left shifts.  */
/* { dg-final { scan-assembler-times {vshl.s[0-9]+\tq[0-9]+, q[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vshl.u[0-9]+\tq[0-9]+, q[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vneg.s[0-9]+\tq[0-9]+, q[0-9]+} 6 } } */


/* Shift by immediate.  */
/* { dg-final { scan-assembler-times {vshr.s[0-9]+\tq[0-9]+, q[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vshr.u[0-9]+\tq[0-9]+, q[0-9]+} 3 } } */
