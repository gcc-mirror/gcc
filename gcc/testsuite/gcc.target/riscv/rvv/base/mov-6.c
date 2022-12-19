/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** mov4:
**	vl1re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (double *in, double *out)
{
  vfloat64m1_t v = *(vfloat64m1_t*)in;
  *(vfloat64m1_t*)out = v;
}

/*
** mov5:
**	vl2re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (double *in, double *out)
{
  vfloat64m2_t v = *(vfloat64m2_t*)in;
  *(vfloat64m2_t*)out = v;
}

/*
** mov6:
**	vl4re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (double *in, double *out)
{
  vfloat64m4_t v = *(vfloat64m4_t*)in;
  *(vfloat64m4_t*)out = v;
}

/*
** mov7:
**	vl8re64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (double *in, double *out)
{
  vfloat64m8_t v = *(vfloat64m8_t*)in;
  *(vfloat64m8_t*)out = v;
}
