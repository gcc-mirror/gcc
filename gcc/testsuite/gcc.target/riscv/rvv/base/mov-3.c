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
void mov3 (int32_t *in, int32_t *out)
{
  vint32mf2_t v = *(vint32mf2_t*)in;
  *(vint32mf2_t*)out = v;
}

/*
** mov4:
**	vl1re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (int32_t *in, int32_t *out)
{
  vint32m1_t v = *(vint32m1_t*)in;
  *(vint32m1_t*)out = v;
}

/*
** mov5:
**	vl2re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (int32_t *in, int32_t *out)
{
  vint32m2_t v = *(vint32m2_t*)in;
  *(vint32m2_t*)out = v;
}

/*
** mov6:
**	vl4re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (int32_t *in, int32_t *out)
{
  vint32m4_t v = *(vint32m4_t*)in;
  *(vint32m4_t*)out = v;
}

/*
** mov7:
**	vl8re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (int32_t *in, int32_t *out)
{
  vint32m8_t v = *(vint32m8_t*)in;
  *(vint32m8_t*)out = v;
}

/*
** mov8:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (uint32_t *in, uint32_t *out)
{
  vuint32mf2_t v = *(vuint32mf2_t*)in;
  *(vuint32mf2_t*)out = v;
}

/*
** mov9:
**	vl1re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (uint32_t *in, uint32_t *out)
{
  vuint32m1_t v = *(vuint32m1_t*)in;
  *(vuint32m1_t*)out = v;
}

/*
** mov10:
**	vl2re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov10 (uint32_t *in, uint32_t *out)
{
  vuint32m2_t v = *(vuint32m2_t*)in;
  *(vuint32m2_t*)out = v;
}

/*
** mov11:
**	vl4re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov11 (uint32_t *in, uint32_t *out)
{
  vuint32m4_t v = *(vuint32m4_t*)in;
  *(vuint32m4_t*)out = v;
}

/*
** mov12:
**	vl8re32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov12 (uint32_t *in, uint32_t *out)
{
  vuint32m8_t v = *(vuint32m8_t*)in;
  *(vuint32m8_t*)out = v;
}
