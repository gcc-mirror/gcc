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
FUNC(s, int, 32, 2, <<, vshl)
FUNC(u, uint, 32, 2, <<, vshl)
FUNC(s, int, 16, 4, <<, vshl)
FUNC(u, uint, 16, 4, <<, vshl)
FUNC(s, int, 8, 8, <<, vshl)
FUNC(u, uint, 8, 8, <<, vshl)

/* 128-bit vectors.  */
FUNC(s, int, 32, 4, <<, vshl)
FUNC(u, uint, 32, 4, <<, vshl)
FUNC(s, int, 16, 8, <<, vshl)  /* FIXME: not vectorized */
FUNC(u, uint, 16, 8, <<, vshl) /* FIXME: not vectorized */
FUNC(s, int, 8, 16, <<, vshl)  /* FIXME: not vectorized */
FUNC(u, uint, 8, 16, <<, vshl) /* FIXME: not vectorized */

/* 64-bit vectors.  */
FUNC_IMM(s, int, 32, 2, <<, vshlimm)
FUNC_IMM(u, uint, 32, 2, <<, vshlimm)
FUNC_IMM(s, int, 16, 4, <<, vshlimm)
FUNC_IMM(u, uint, 16, 4, <<, vshlimm)
FUNC_IMM(s, int, 8, 8, <<, vshlimm)
FUNC_IMM(u, uint, 8, 8, <<, vshlimm)

/* 128-bit vectors.  */
FUNC_IMM(s, int, 32, 4, <<, vshlimm)
FUNC_IMM(u, uint, 32, 4, <<, vshlimm)
FUNC_IMM(s, int, 16, 8, <<, vshlimm)
FUNC_IMM(u, uint, 16, 8, <<, vshlimm)
FUNC_IMM(s, int, 8, 16, <<, vshlimm)
FUNC_IMM(u, uint, 8, 16, <<, vshlimm)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* We only emit vshl.u, which is equivalent to vshl.s anyway.  */
/* 16 and 8-bit versions still use 32-bit intermediate temporaries, so for
   instance instead of using vshl.u8, we need 4 vshl.i32, leading to a total of
   14 vshl.i32 expected in this testcase.  */
/* { dg-final { scan-assembler-times {vshl.u[0-9]+\tq[0-9]+, q[0-9]+} 14 } } */

/* We emit vshl.i when the shift amount is an immediate.  */
/* { dg-final { scan-assembler-times {vshl.i[0-9]+\tq[0-9]+, q[0-9]+} 6 } } */
