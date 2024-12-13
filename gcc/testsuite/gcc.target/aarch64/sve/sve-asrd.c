/* { dg-do compile } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=asimd-only" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#define FUNC(TYPE, I)                                                          \
  TYPE M_##TYPE##_##I[I];                                                      \
  void asrd_##TYPE##_##I ()                                                    \
  {                                                                            \
    for (int i = 0; i < I; i++)                                                \
      {                                                                        \
	M_##TYPE##_##I[i] /= 4;                                                \
      }                                                                        \
  }

/*
** asrd_int8_t_8:
**	...
**	ptrue	(p[0-7]).b, vl8
**	...
**	asrd	z[0-9]+\.b, \1/m, z[0-9]+\.b, #2
**	...
*/
FUNC(int8_t, 8);

/*
** asrd_int8_t_16:
**	...
**	ptrue	(p[0-7]).b, vl16
**	...
**	asrd	z[0-9]+\.b, \1/m, z[0-9]+\.b, #2
**	...
*/
FUNC(int8_t, 16);

/*
** asrd_int16_t_4:
**	...
**	ptrue	(p[0-7]).b, vl8
**	...
**	asrd	z[0-9]+\.h, \1/m, z[0-9]+\.h, #2
**	...
*/
FUNC(int16_t, 4);

/*
** asrd_int16_t_8:
**	...
**	ptrue	(p[0-7]).b, vl16
**	...
**	asrd	z[0-9]+\.h, \1/m, z[0-9]+\.h, #2
**	...
*/
FUNC(int16_t, 8);

/*
** asrd_int32_t_2:
**	...
**	ptrue	(p[0-7]).b, vl8
**	...
**	asrd	z[0-9]+\.s, \1/m, z[0-9]+\.s, #2
**	...
*/
FUNC(int32_t, 2);

/*
** asrd_int32_t_4:
**	...
**	ptrue	(p[0-7]).b, vl16
**	...
**	asrd	z[0-9]+\.s, \1/m, z[0-9]+\.s, #2
**	...
*/
FUNC(int32_t, 4);

/*
** asrd_int64_t_2:
**	...
**	ptrue	(p[0-7]).b, vl16
**	...
**	asrd	z[0-9]+\.d, \1/m, z[0-9]+\.d, #2
**	...
*/
FUNC(int64_t, 2);

