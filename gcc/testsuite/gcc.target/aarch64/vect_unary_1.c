/* { dg-options "-O3 -fno-math-errno --save-temps" } */
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
}									\

#define TEST4(OUT, NAME, IN)						\
OUT __attribute__((vector_size(16)))					\
test4_##OUT##_##NAME##_##IN (float dummy,				\
			     IN __attribute__((vector_size(16))) y)	\
{									\
  OUT __attribute__((vector_size(16))) x;				\
  x[0] = __builtin_##NAME (y[0]);					\
  x[1] = __builtin_##NAME (y[1]);					\
  x[2] = __builtin_##NAME (y[2]);					\
  x[3] = __builtin_##NAME (y[3]);					\
  return x;								\
}									\

/*
** test2_float_truncf_float:
**	frintz	v0.2s, v1.2s
**	ret
*/
TEST2 (float, truncf, float)

/*
** test2_double_trunc_double:
**	frintz	v0.2d, v1.2d
**	ret
*/
TEST2 (double, trunc, double)

/*
** test4_float_truncf_float:
**	frintz	v0.4s, v1.4s
**	ret
*/
TEST4 (float, truncf, float)

/*
** test2_float_roundf_float:
**	frinta	v0.2s, v1.2s
**	ret
*/
TEST2 (float, roundf, float)

/*
** test2_double_round_double:
**	frinta	v0.2d, v1.2d
**	ret
*/
TEST2 (double, round, double)

/*
** test4_float_roundf_float:
**	frinta	v0.4s, v1.4s
**	ret
*/
TEST4 (float, roundf, float)

/*
** test2_float_nearbyintf_float:
**	frinti	v0.2s, v1.2s
**	ret
*/
TEST2 (float, nearbyintf, float)

/*
** test2_double_nearbyint_double:
**	frinti	v0.2d, v1.2d
**	ret
*/
TEST2 (double, nearbyint, double)

/*
** test4_float_nearbyintf_float:
**	frinti	v0.4s, v1.4s
**	ret
*/
TEST4 (float, nearbyintf, float)

/*
** test2_float_floorf_float:
**	frintm	v0.2s, v1.2s
**	ret
*/
TEST2 (float, floorf, float)

/*
** test2_double_floor_double:
**	frintm	v0.2d, v1.2d
**	ret
*/
TEST2 (double, floor, double)

/*
** test4_float_floorf_float:
**	frintm	v0.4s, v1.4s
**	ret
*/
TEST4 (float, floorf, float)

/*
** test2_float_ceilf_float:
**	frintp	v0.2s, v1.2s
**	ret
*/
TEST2 (float, ceilf, float)

/*
** test2_double_ceil_double:
**	frintp	v0.2d, v1.2d
**	ret
*/
TEST2 (double, ceil, double)

/*
** test4_float_ceilf_float:
**	frintp	v0.4s, v1.4s
**	ret
*/
TEST4 (float, ceilf, float)

/*
** test2_float_rintf_float:
**	frintx	v0.2s, v1.2s
**	ret
*/
TEST2 (float, rintf, float)

/*
** test2_double_rint_double:
**	frintx	v0.2d, v1.2d
**	ret
*/
TEST2 (double, rint, double)

/*
** test4_float_rintf_float:
**	frintx	v0.4s, v1.4s
**	ret
*/
TEST4 (float, rintf, float)

/*
** test2_int_clz_int:
**	clz	v0.2s, v1.2s
**	ret
*/
TEST2 (int, clz, int)

/*
** test4_int_clz_int:
**	clz	v0.4s, v1.4s
**	ret
*/
TEST4 (int, clz, int)

/*
** test2_int_ctz_int:
**	rev32	(v[0-9]+).8b, v1.8b
**	rbit	(v[0-9]+).8b, \1.8b
**	clz	v0.2s, \2.2s
**	ret
*/
TEST2 (int, ctz, int)

/*
** test4_int_ctz_int:
**	rev32	(v[0-9]+).16b, v1.16b
**	rbit	(v[0-9]+).16b, \1.16b
**	clz	v0.4s, \2.4s
**	ret
*/
TEST4 (int, ctz, int)

/*
** test2_int_iroundf_float:
**	fcvtas	v0.2s, v1.2s
**	ret
*/
TEST2 (int, iroundf, float)

/*
** test2_int64_t_llround_double:
**	fcvtas	v0.2d, v1.2d
**	ret
*/
TEST2 (int64_t, llround, double)

/*
** test4_int_iroundf_float:
**	fcvtas	v0.4s, v1.4s
**	ret
*/
TEST4 (int, iroundf, float)

/*
** test2_int_ifloorf_float:
**	fcvtms	v0.2s, v1.2s
**	ret
*/
TEST2 (int, ifloorf, float)

/*
** test2_int64_t_llfloor_double:
**	fcvtms	v0.2d, v1.2d
**	ret
*/
TEST2 (int64_t, llfloor, double)

/*
** test4_int_ifloorf_float:
**	fcvtms	v0.4s, v1.4s
**	ret
*/
TEST4 (int, ifloorf, float)

/*
** test2_int_iceilf_float:
**	fcvtps	v0.2s, v1.2s
**	ret
*/
TEST2 (int, iceilf, float)

/*
** test2_int64_t_llceil_double:
**	fcvtps	v0.2d, v1.2d
**	ret
*/
TEST2 (int64_t, llceil, double)

/*
** test4_int_iceilf_float:
**	fcvtps	v0.4s, v1.4s
**	ret
*/
TEST4 (int, iceilf, float)
