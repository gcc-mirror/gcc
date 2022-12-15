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

#define ALL_FUNCS(OP, NAME) \
  FUNC(s, int, 32, 2, OP, NAME)			\
  FUNC(u, uint, 32, 2, OP, NAME)		\
  FUNC(s, int, 16, 4, OP, NAME)			\
  FUNC(u, uint, 16, 4, OP, NAME)		\
  FUNC(s, int, 8, 8, OP, NAME)			\
  FUNC(u, uint, 8, 8, OP, NAME)			\
  FUNC(s, int, 32, 4, OP, NAME)			\
  FUNC(u, uint, 32, 4, OP, NAME)		\
  FUNC(s, int, 16, 8, OP, NAME)			\
  FUNC(u, uint, 16, 8, OP, NAME)		\
  FUNC(s, int, 8, 16, OP, NAME)			\
  FUNC(u, uint, 8, 16, OP, NAME)

ALL_FUNCS(==, vcmpeq)
ALL_FUNCS(!=, vcmpne)
ALL_FUNCS(<, vcmplt)
ALL_FUNCS(<=, vcmple)
ALL_FUNCS(>, vcmpgt)
ALL_FUNCS(>=, vcmpge)

/* MVE has only 128-bit vectors, so we can vectorize only half of the
   functions above.  */
/* { dg-final { scan-assembler-times {\tvcmp.i[0-9]+\teq, q[0-9]+, q[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i[0-9]+\tne, q[0-9]+, q[0-9]+\n} 6 } } */

/* lt, le, gt, ge apply to signed types, cs and hi to unsigned types.  */
/* lt and le with unsigned types are replaced with the opposite condition, hence
   the double number of matches for cs and hi.  */
/* { dg-final { scan-assembler-times {\tvcmp.s[0-9]+\tlt, q[0-9]+, q[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s[0-9]+\tle, q[0-9]+, q[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s[0-9]+\tgt, q[0-9]+, q[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s[0-9]+\tge, q[0-9]+, q[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u[0-9]+\tcs, q[0-9]+, q[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u[0-9]+\thi, q[0-9]+, q[0-9]+\n} 6 } } */
