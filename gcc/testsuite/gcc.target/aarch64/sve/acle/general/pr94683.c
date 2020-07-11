/* { dg-options "-O2 -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

typedef float v8sf __attribute__((vector_size(32)));

#ifdef __cplusplus
extern "C" {
#endif

/*
** test:
**	fadd	z0\.s, p0/m, z0\.s, #1.0
**	trn1	z0\.s, z0\.s, z0\.s
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
svfloat32_t
test (svbool_t pg, svfloat32_t x, svfloat32_t y)
{
  v8sf a = svadd_x (pg, x, 1);
  v8sf b = { a[0], a[0], a[2], a[2], a[4], a[4], a[6], a[6] };
  return svdiv_x (pg, b, y);
}

#ifdef __cplusplus
}
#endif
