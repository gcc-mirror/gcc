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
void mov1 (int64_t *in, int64_t *out)
{
  v2di v = *(v2di*)in;
  *(v2di*)out = v;
}

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov2 (int64_t *in, int64_t *out)
{
  v4di v = *(v4di*)in;
  *(v4di*)out = v;
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov3 (int64_t *in, int64_t *out)
{
  v8di v = *(v8di*)in;
  *(v8di*)out = v;
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov4 (int64_t *in, int64_t *out)
{
  v16di v = *(v16di*)in;
  *(v16di*)out = v;
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov5 (int64_t *in, int64_t *out)
{
  v32di v = *(v32di*)in;
  *(v32di*)out = v;
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov6 (int64_t *in, int64_t *out)
{
  v64di v = *(v64di*)in;
  *(v64di*)out = v;
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov7 (int64_t *in, int64_t *out)
{
  v128di v = *(v128di*)in;
  *(v128di*)out = v;
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov8 (int64_t *in, int64_t *out)
{
  v256di v = *(v256di*)in;
  *(v256di*)out = v;
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]
**	vle64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**	vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void mov9 (int64_t *in, int64_t *out)
{
  v512di v = *(v512di*)in;
  *(v512di*)out = v;
}
