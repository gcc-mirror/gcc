/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O3 --param aarch64-autovec-preference=sve-only" } */

#include <stdint.h>

#define FUNC(FUNCTION, NAME, DTYPE)                 \
void                                                     \
NAME (DTYPE *__restrict x, DTYPE *__restrict y, int n) { \
  for (int i = 0; i < n; i++)                            \
    x[i] = FUNCTION (y[i]);                         \
}                                                        \


/*
** ctz_uint8:
**	...
**	rbit	z[0-9]+\.b, p[0-7]/m, z[0-9]+\.b
**	clz	z[0-9]+\.b, p[0-7]/m, z[0-9]+\.b
**	...
*/
FUNC (__builtin_ctzg, ctz_uint8, uint8_t)

/*
** ctz_uint16:
**	...
**	rbit	z[0-9]+\.h, p[0-7]/m, z[0-9]+\.h
**	clz	z[0-9]+\.h, p[0-7]/m, z[0-9]+\.h
**	...
*/
FUNC (__builtin_ctzg, ctz_uint16, uint16_t)

/*
** ctz_uint32:
**	...
**	rbit	z[0-9]+\.s, p[0-7]/m, z[0-9]+\.s
**	clz	z[0-9]+\.s, p[0-7]/m, z[0-9]+\.s
**	...
*/
FUNC (__builtin_ctz, ctz_uint32, uint32_t)

/*
** ctz_uint64:
**	...
**	rbit	z[0-9]+\.d, p[0-7]/m, z[0-9]+\.d
**	clz	z[0-9]+\.d, p[0-7]/m, z[0-9]+\.d
**	...
*/
FUNC (__builtin_ctzll, ctz_uint64, uint64_t)

