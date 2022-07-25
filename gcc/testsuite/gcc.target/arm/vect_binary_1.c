/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-add-options arm_v8_neon }  */
/* { dg-additional-options "-O3 -mfloat-abi=hard" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#define TEST2(OUT, NAME, IN)						\
OUT __attribute__((vector_size(sizeof(OUT) * 2)))			\
test2_##OUT##_##NAME##_##IN (float dummy,				\
			     IN __attribute__((vector_size(sizeof(IN) * 2))) y, \
			     IN __attribute__((vector_size(sizeof(IN) * 2))) z) \
{									\
  OUT __attribute__((vector_size(sizeof(OUT) * 2))) x;			\
  x[0] = __builtin_##NAME (y[0], z[0]);					\
  x[1] = __builtin_##NAME (y[1], z[1]);					\
  return x;								\
}

#define TEST4(OUT, NAME, IN)						\
OUT __attribute__((vector_size(sizeof(OUT) * 4)))			\
test4_##OUT##_##NAME##_##IN (float dummy,				\
			     IN __attribute__((vector_size(sizeof(OUT) * 4))) y,	\
			     IN __attribute__((vector_size(sizeof(OUT) * 4))) z)	\
{									\
  OUT __attribute__((vector_size(sizeof(OUT) * 4))) x;			\
  x[0] = __builtin_##NAME (y[0], z[0]);					\
  x[1] = __builtin_##NAME (y[1], z[1]);					\
  x[2] = __builtin_##NAME (y[2], z[2]);					\
  x[3] = __builtin_##NAME (y[3], z[3]);					\
  return x;								\
}

/*
** test2_float_copysignf_float: { target arm_little_endian }
**	vmov.i32	d0, #(0x80000000|2147483648)(\s+.*)
**	vbsl	d0, d2, d1
**	bx	lr
*/
TEST2 (float, copysignf, float)

/*
** test4_float_copysignf_float: { target arm_little_endian }
**	vmov.i32	q0, #(0x80000000|2147483648)(\s+.*)
**	vbsl	q0, q2, q1
**	bx	lr
*/
TEST4 (float, copysignf, float)
