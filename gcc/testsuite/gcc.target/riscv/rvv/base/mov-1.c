/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** mov1:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov1 (int8_t *in, int8_t *out)
{
  vint8mf8_t v = *(vint8mf8_t*)in;
  *(vint8mf8_t*)out = v;
}

/*
** mov2:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov2 (int8_t *in, int8_t *out)
{
  vint8mf4_t v = *(vint8mf4_t*)in;
  *(vint8mf4_t*)out = v;
}

/*
** mov3:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (int8_t *in, int8_t *out)
{
  vint8mf2_t v = *(vint8mf2_t*)in;
  *(vint8mf2_t*)out = v;
}

/*
** mov4:
**	vl1re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (int8_t *in, int8_t *out)
{
  vint8m1_t v = *(vint8m1_t*)in;
  *(vint8m1_t*)out = v;
}

/*
** mov5:
**	vl2re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (int8_t *in, int8_t *out)
{
  vint8m2_t v = *(vint8m2_t*)in;
  *(vint8m2_t*)out = v;
}

/*
** mov6:
**	vl4re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (int8_t *in, int8_t *out)
{
  vint8m4_t v = *(vint8m4_t*)in;
  *(vint8m4_t*)out = v;
}

/*
** mov7:
**	vl8re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (int8_t *in, int8_t *out)
{
  vint8m8_t v = *(vint8m8_t*)in;
  *(vint8m8_t*)out = v;
}

/*
** mov8:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (uint8_t *in, uint8_t *out)
{
  vuint8mf8_t v = *(vuint8mf8_t*)in;
  *(vuint8mf8_t*)out = v;
}

/*
** mov9:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (uint8_t *in, uint8_t *out)
{
  vuint8mf4_t v = *(vuint8mf4_t*)in;
  *(vuint8mf4_t*)out = v;
}

/*
** mov10:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]
**	vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov10 (uint8_t *in, uint8_t *out)
{
  vuint8mf2_t v = *(vuint8mf2_t*)in;
  *(vuint8mf2_t*)out = v;
}

/*
** mov11:
**	vl1re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov11 (uint8_t *in, uint8_t *out)
{
  vuint8m1_t v = *(vuint8m1_t*)in;
  *(vuint8m1_t*)out = v;
}

/*
** mov12:
**	vl2re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov12 (uint8_t *in, uint8_t *out)
{
  vuint8m2_t v = *(vuint8m2_t*)in;
  *(vuint8m2_t*)out = v;
}

/*
** mov13:
**	vl4re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov13 (uint8_t *in, uint8_t *out)
{
  vuint8m4_t v = *(vuint8m4_t*)in;
  *(vuint8m4_t*)out = v;
}

/*
** mov14:
**	vl8re8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov14 (uint8_t *in, uint8_t *out)
{
  vuint8m8_t v = *(vuint8m8_t*)in;
  *(vuint8m8_t*)out = v;
}
