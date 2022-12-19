/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** mov2:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov2 (int16_t *in, int16_t *out)
{
  vint16mf4_t v = *(vint16mf4_t*)in;
  *(vint16mf4_t*)out = v;
}

/*
** mov3:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]
**	vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (int16_t *in, int16_t *out)
{
  vint16mf2_t v = *(vint16mf2_t*)in;
  *(vint16mf2_t*)out = v;
}

/*
** mov4:
**	vl1re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (int16_t *in, int16_t *out)
{
  vint16m1_t v = *(vint16m1_t*)in;
  *(vint16m1_t*)out = v;
}

/*
** mov5:
**	vl2re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (int16_t *in, int16_t *out)
{
  vint16m2_t v = *(vint16m2_t*)in;
  *(vint16m2_t*)out = v;
}

/*
** mov6:
**	vl4re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (int16_t *in, int16_t *out)
{
  vint16m4_t v = *(vint16m4_t*)in;
  *(vint16m4_t*)out = v;
}

/*
** mov7:
**	vl8re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (int16_t *in, int16_t *out)
{
  vint16m8_t v = *(vint16m8_t*)in;
  *(vint16m8_t*)out = v;
}

/*
** mov8:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (uint16_t *in, uint16_t *out)
{
  vuint16mf4_t v = *(vuint16mf4_t*)in;
  *(vuint16mf4_t*)out = v;
}

/*
** mov9:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]
**	vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (uint16_t *in, uint16_t *out)
{
  vuint16mf2_t v = *(vuint16mf2_t*)in;
  *(vuint16mf2_t*)out = v;
}

/*
** mov10:
**	vl1re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov10 (uint16_t *in, uint16_t *out)
{
  vuint16m1_t v = *(vuint16m1_t*)in;
  *(vuint16m1_t*)out = v;
}

/*
** mov11:
**	vl2re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov11 (uint16_t *in, uint16_t *out)
{
  vuint16m2_t v = *(vuint16m2_t*)in;
  *(vuint16m2_t*)out = v;
}

/*
** mov12:
**	vl4re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov12 (uint16_t *in, uint16_t *out)
{
  vuint16m4_t v = *(vuint16m4_t*)in;
  *(vuint16m4_t*)out = v;
}

/*
** mov13:
**	vl8re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov13 (uint16_t *in, uint16_t *out)
{
  vuint16m8_t v = *(vuint16m8_t*)in;
  *(vuint16m8_t*)out = v;
}
