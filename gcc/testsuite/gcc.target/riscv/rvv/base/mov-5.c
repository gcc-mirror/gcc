/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** mov3:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (float *in, float *out)
{
  vfloat32mf2_t v = *(vfloat32mf2_t*)in;
  *(vfloat32mf2_t*)out = v;
}

/*
** mov4:
**	vl1re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (float *in, float *out)
{
  vfloat32m1_t v = *(vfloat32m1_t*)in;
  *(vfloat32m1_t*)out = v;
}

/*
** mov5:
**	vl2re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (float *in, float *out)
{
  vfloat32m2_t v = *(vfloat32m2_t*)in;
  *(vfloat32m2_t*)out = v;
}

/*
** mov6:
**	vl4re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (float *in, float *out)
{
  vfloat32m4_t v = *(vfloat32m4_t*)in;
  *(vfloat32m4_t*)out = v;
}

/*
** mov7:
**	vl8re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (float *in, float *out)
{
  vfloat32m8_t v = *(vfloat32m8_t*)in;
  *(vfloat32m8_t*)out = v;
}
