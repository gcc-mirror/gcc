/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov1:
**	vsetivli\s+zero,\s*2,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov1 (double *in, double *out)
{
  v2df v = *(v2df*)in;
  *(v2df*)out = v;
}

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov2 (double *in, double *out)
{
  v4df v = *(v4df*)in;
  *(v4df*)out = v;
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (double *in, double *out)
{
  v8df v = *(v8df*)in;
  *(v8df*)out = v;
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (double *in, double *out)
{
  v16df v = *(v16df*)in;
  *(v16df*)out = v;
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (double *in, double *out)
{
  v32df v = *(v32df*)in;
  *(v32df*)out = v;
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (double *in, double *out)
{
  v64df v = *(v64df*)in;
  *(v64df*)out = v;
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (double *in, double *out)
{
  v128df v = *(v128df*)in;
  *(v128df*)out = v;
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (double *in, double *out)
{
  v256df v = *(v256df*)in;
  *(v256df*)out = v;
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (double *in, double *out)
{
  v512df v = *(v512df*)in;
  *(v512df*)out = v;
}
