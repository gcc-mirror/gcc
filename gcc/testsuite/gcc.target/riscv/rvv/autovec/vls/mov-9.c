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
void mov2 (float *in, float *out)
{
  v4sf v = *(v4sf*)in;
  *(v4sf*)out = v;
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (float *in, float *out)
{
  v8sf v = *(v8sf*)in;
  *(v8sf*)out = v;
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (float *in, float *out)
{
  v16sf v = *(v16sf*)in;
  *(v16sf*)out = v;
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (float *in, float *out)
{
  v32sf v = *(v32sf*)in;
  *(v32sf*)out = v;
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (float *in, float *out)
{
  v64sf v = *(v64sf*)in;
  *(v64sf*)out = v;
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (float *in, float *out)
{
  v128sf v = *(v128sf*)in;
  *(v128sf*)out = v;
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (float *in, float *out)
{
  v256sf v = *(v256sf*)in;
  *(v256sf*)out = v;
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (float *in, float *out)
{
  v512sf v = *(v512sf*)in;
  *(v512sf*)out = v;
}

/*
** mov10:
**	li\s+[a-x0-9]+,1024
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]
**	vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov10 (float *in, float *out)
{
  v1024sf v = *(v1024sf*)in;
  *(v1024sf*)out = v;
}
