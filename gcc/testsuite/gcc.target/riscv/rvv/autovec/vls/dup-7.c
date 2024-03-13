/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-builtin -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** foo1:
**  vsetivli\s+zero,\s*4,\s*e64,\s*m1,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo1 (double *in, double *out, double x)
{
  for (int i = 0; i < 4; i++)
    in[i] = x;
}

/*
** foo2:
**  vsetivli\s+zero,\s*8,\s*e64,\s*m1,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo2 (double *in, double *out, double x)
{
  for (int i = 0; i < 8; i++)
    in[i] = x;
}

/*
** foo3:
**  vsetivli\s+zero,\s*16,\s*e64,\s*m1,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo3 (double *in, double *out, double x)
{
  for (int i = 0; i < 16; i++)
    in[i] = x;
}

/*
** foo4:
**  li\s+[a-x0-9]+,32
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo4 (double *in, double *out, double x)
{
  for (int i = 0; i < 32; i++)
    in[i] = x;
}

/*
** foo5:
**  li\s+[a-x0-9]+,64
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo5 (double *in, double *out, double x)
{
  for (int i = 0; i < 64; i++)
    in[i] = x;
}

/*
** foo6:
**  li\s+[a-x0-9]+,128
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo6 (double *in, double *out, double x)
{
  for (int i = 0; i < 128; i++)
    in[i] = x;
}

/*
** foo7:
**  li\s+[a-x0-9]+,256
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo7 (double *in, double *out, double x)
{
  for (int i = 0; i < 256; i++)
    in[i] = x;
}

/*
** foo8:
**  li\s+[a-x0-9]+,512
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]
**  vfmv\.v\.f\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse64\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo8 (double *in, double *out, double x)
{
  for (int i = 0; i < 512; i++)
    in[i] = x;
}
