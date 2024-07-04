/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov2 (int32_t *in, int32_t *out)
{
  v4si v = *(v4si*)in;
  *(v4si*)out = v;
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (int32_t *in, int32_t *out)
{
  v8si v = *(v8si*)in;
  *(v8si*)out = v;
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (int32_t *in, int32_t *out)
{
  v16si v = *(v16si*)in;
  *(v16si*)out = v;
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (int32_t *in, int32_t *out)
{
  v32si v = *(v32si*)in;
  *(v32si*)out = v;
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (int32_t *in, int32_t *out)
{
  v64si v = *(v64si*)in;
  *(v64si*)out = v;
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (int32_t *in, int32_t *out)
{
  v128si v = *(v128si*)in;
  *(v128si*)out = v;
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (int32_t *in, int32_t *out)
{
  v256si v = *(v256si*)in;
  *(v256si*)out = v;
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (int32_t *in, int32_t *out)
{
  v512si v = *(v512si*)in;
  *(v512si*)out = v;
}

/*
** mov10:
**	li\s+[a-x0-9]+,1024
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov10 (int32_t *in, int32_t *out)
{
  v1024si v = *(v1024si*)in;
  *(v1024si*)out = v;
}
