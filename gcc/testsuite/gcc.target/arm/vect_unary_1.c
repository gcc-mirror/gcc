/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-add-options arm_v8_neon }  */
/* { dg-additional-options "-Ofast -mfloat-abi=hard" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#define TEST2(OUT, NAME, IN)						\
OUT __attribute__((vector_size(sizeof(OUT) * 2)))			\
test2_##OUT##_##NAME##_##IN (float dummy,				\
			     IN __attribute__((vector_size(sizeof(IN) * 2))) y) \
{									\
  OUT __attribute__((vector_size(sizeof(OUT) * 2))) x;			\
  x[0] = __builtin_##NAME (y[0]);					\
  x[1] = __builtin_##NAME (y[1]);					\
  return x;								\
}

#define TEST4(OUT, NAME, IN)						\
OUT __attribute__((vector_size(sizeof(OUT) * 4)))			\
test4_##OUT##_##NAME##_##IN (float dummy,				\
			     IN __attribute__((vector_size(sizeof(OUT) * 4))) y)	\
{									\
  OUT __attribute__((vector_size(sizeof(OUT) * 4))) x;			\
  x[0] = __builtin_##NAME (y[0]);					\
  x[1] = __builtin_##NAME (y[1]);					\
  x[2] = __builtin_##NAME (y[2]);					\
  x[3] = __builtin_##NAME (y[3]);					\
  return x;								\
}

#define TEST8(OUT, NAME, IN)						\
OUT __attribute__((vector_size(sizeof(OUT) * 8)))			\
test8_##OUT##_##NAME##_##IN (float dummy,				\
			     IN __attribute__((vector_size(sizeof(OUT) * 8))) y)	\
{									\
  OUT __attribute__((vector_size(sizeof(OUT) * 8))) x;			\
  x[0] = __builtin_##NAME (y[0]);					\
  x[1] = __builtin_##NAME (y[1]);					\
  x[2] = __builtin_##NAME (y[2]);					\
  x[3] = __builtin_##NAME (y[3]);					\
  x[4] = __builtin_##NAME (y[4]);					\
  x[5] = __builtin_##NAME (y[5]);					\
  x[6] = __builtin_##NAME (y[6]);					\
  x[7] = __builtin_##NAME (y[7]);					\
  return x;								\
}

/*
** test2_float_truncf_float:
**	vrintz.f32	d0, d1
**	bx	lr
*/
TEST2 (float, truncf, float)

/*
** test4_float_truncf_float:
**	vrintz.f32	q0, q1
**	bx	lr
*/
TEST4 (float, truncf, float)

/*
** test2_float_roundf_float:
**	vrinta.f32	d0, d1
**	bx	lr
*/
TEST2 (float, roundf, float)

/*
** test4_float_roundf_float:
**	vrinta.f32	q0, q1
**	bx	lr
*/
TEST4 (float, roundf, float)

/*
** test2_float_floorf_float:
**	vrintm.f32	d0, d1
**	bx	lr
*/
TEST2 (float, floorf, float)

/*
** test4_float_floorf_float:
**	vrintm.f32	q0, q1
**	bx	lr
*/
TEST4 (float, floorf, float)

/*
** test2_float_ceilf_float:
**	vrintp.f32	d0, d1
**	bx	lr
*/
TEST2 (float, ceilf, float)

/*
** test4_float_ceilf_float:
**	vrintp.f32	q0, q1
**	bx	lr
*/
TEST4 (float, ceilf, float)

/*
** test2_float_rintf_float:
**	vrintx.f32	d0, d1
**	bx	lr
*/
TEST2 (float, rintf, float)

/*
** test4_float_rintf_float:
**	vrintx.f32	q0, q1
**	bx	lr
*/
TEST4 (float, rintf, float)

/*
** test2_float_roundevenf_float:
**	vrintn.f32	d0, d1
**	bx	lr
*/
TEST2 (float, roundevenf, float)

/*
** test4_float_roundevenf_float:
**	vrintn.f32	q0, q1
**	bx	lr
*/
TEST4 (float, roundevenf, float)

/*
** test2_int_roundf_float:
**	vcvta.s32.f32	d0, d1
**	bx	lr
*/
TEST2 (int, roundf, float)

/*
** test4_int_roundf_float:
**	vcvta.s32.f32	q0, q1
**	bx	lr
*/
TEST4 (int, roundf, float)

/*
** test2_int_floorf_float:
**	vcvtm.s32.f32	d0, d1
**	bx	lr
*/
TEST2 (int, floorf, float)

/*
** test4_int_floorf_float:
**	vcvtm.s32.f32	q0, q1
**	bx	lr
*/
TEST4 (int, floorf, float)

/*
** test2_int_ceilf_float:
**	vcvtp.s32.f32	d0, d1
**	bx	lr
*/
TEST2 (int, ceilf, float)

/*
** test4_int_ceilf_float:
**	vcvtp.s32.f32	q0, q1
**	bx	lr
*/
TEST4 (int, ceilf, float)

/*
** test2_int_clz_int:
**	vclz.i32	d0, d1
**	bx	lr
*/
TEST2 (int, clz, int)

/*
** test4_int_clz_int:
**	vclz.i32	q0, q1
**	bx	lr
*/
TEST4 (int, clz, int)

/*
** test4_int16_t_bswap16_int16_t: { target arm_little_endian }
**	vrev16.8	d0, d1
**	bx	lr
*/
TEST4 (int16_t, bswap16, int16_t)

/*
** test8_int16_t_bswap16_int16_t: { target arm_little_endian }
**	vrev16.8	q0, q1
**	bx	lr
*/
TEST8 (int16_t, bswap16, int16_t)

/*
** test2_int_bswap32_int: { target arm_little_endian }
**	vrev32.8	d0, d1
**	bx	lr
*/
TEST2 (int, bswap32, int)

/*
** test4_int_bswap32_int: { target arm_little_endian }
**	vrev32.8	q0, q1
**	bx	lr
*/
TEST4 (int, bswap32, int)

/*
** test2_int64_t_bswap64_int64_t: { target arm_little_endian }
**	vrev64.8	q0, q1
**	bx	lr
*/
TEST2 (int64_t, bswap64, int64_t)
