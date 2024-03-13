/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-builtin -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** foo1:
**  vsetivli\s+zero,\s*4,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo1 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 4; i++)
    in[i] = x;
}

/*
** foo2:
**  vsetivli\s+zero,\s*8,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo2 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 8; i++)
    in[i] = x;
}

/*
** foo3:
**  vsetivli\s+zero,\s*16,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo3 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 16; i++)
    in[i] = x;
}

/*
** foo4:
**  li\s+[a-x0-9]+,32
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo4 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 32; i++)
    in[i] = x;
}

/*
** foo5:
**  li\s+[a-x0-9]+,64
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo5 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 64; i++)
    in[i] = x;
}

/*
** foo6:
**  li\s+[a-x0-9]+,128
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo6 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 128; i++)
    in[i] = x;
}

/*
** foo7:
**  li\s+[a-x0-9]+,256
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo7 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 256; i++)
    in[i] = x;
}

/*
** foo8:
**  li\s+[a-x0-9]+,512
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo8 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 512; i++)
    in[i] = x;
}

/*
** foo9:
**  li\s+[a-x0-9]+,1024
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo9 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 1024; i++)
    in[i] = x;
}

/*
** foo10:
**  li\s+[a-x0-9]+,4096
**  addi\s+[a-x0-9]+,[a-x0-9]+,-2048
**  vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]
**  vmv\.v\.x\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),\s*[a-x0-9]+
**  vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**  ret
*/
void
foo10 (int16_t *in, int16_t *out, int16_t x)
{
  for (int i = 0; i < 2048; i++)
    in[i] = x;
}
