/* { dg-options "-O2 -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))
#define GNU_ATTR __attribute__ ((vector_size (N / 8)))

typedef svint8_t fixed_int8_t FIXED_ATTR;

typedef svbool_t fixed_bool_t FIXED_ATTR;

typedef int8_t gnu_int8_t GNU_ATTR;

#ifdef __cplusplus
extern "C" {
#endif

/*
** test_add:
**	add	z0\.b, (?:z0\.b, z1\.b|z1\.b, z0\.b)
**	ret
*/
fixed_int8_t
test_add (fixed_int8_t x, fixed_int8_t y)
{
  return x + y;
}

/*
** test_add_gnu:
** (
**	add	(z[0-9]+\.b), (?:z0\.b, z1\.b|z1\.b, z0\.b)
**	ptrue	(p[0-7])\.b, vl32
**	st1b	\1, \2, \[x8\]
** |
**	ptrue	(p[0-7]\.b), vl32
**	add	(z[0-9]+)\.b, (?:z0\.b, z1\.b|z1\.b, z0\.b)
**	st1b	\4, \3, \[x8\]
** )
**	ret
*/
gnu_int8_t
test_add_gnu (fixed_int8_t x, fixed_int8_t y)
{
  return x + y;
}

/*
** test_load: { target lp64 }
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
/*
** test_load: { target ilp32 }
**	uxtw	x0, w0
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
fixed_int8_t
test_load (fixed_bool_t pg, int8_t *ptr)
{
  return svld1 (pg, ptr);
}

/*
** test_store: { target lp64 }
**	st1b	z0\.b, p0, \[x0\]
**	ret
*/
/*
** test_store: { target ilp32 }
**	uxtw	x0, w0
**	st1b	z0\.b, p0, \[x0\]
**	ret
*/
void
test_store (fixed_bool_t pg, int8_t *ptr, fixed_int8_t data)
{
  svst1 (pg, ptr, data);
}

/*
** test_and_z:
**	and	p0\.b, p0/z, p1\.b, p2\.b
**	ret
*/
fixed_bool_t
test_and_z (fixed_bool_t pg, svbool_t p1, fixed_bool_t p2)
{
  return svand_z (pg, p1, p2);
}

#ifdef __cplusplus
}
#endif
