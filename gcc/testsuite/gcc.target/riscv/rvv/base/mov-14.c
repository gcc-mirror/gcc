/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfhmin -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

typedef _Float16 float16_t;

/*
** mov_vf16_mf4:
**   vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**   vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   ret
*/
void mov_vf16_mf4 (float16_t *in, float16_t *out)
{
  vfloat16mf4_t v = *(vfloat16mf4_t *)in;
  * (vfloat16mf4_t *) out = v;
}

/*
** mov_vf16_mf2:
**   vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]
**   vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   vse16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   ret
*/
void mov_vf16_mf2 (float16_t *in, float16_t *out)
{
  vfloat16mf2_t v = *(vfloat16mf2_t *)in;
  * (vfloat16mf2_t *) out = v;
}

/*
** mov_vf16_m1:
**   vl1re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   vs1r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   ret
*/
void mov_vf16_m1 (float16_t *in, float16_t *out)
{
  vfloat16m1_t v = *(vfloat16m1_t *)in;
  * (vfloat16m1_t *) out = v;
}

/*
** mov_vf16_m2:
**   vl2re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   vs2r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   ret
*/
void mov_vf16_m2 (float16_t *in, float16_t *out)
{
  vfloat16m2_t v = *(vfloat16m2_t *)in;
  * (vfloat16m2_t *) out = v;
}

/*
** mov_vf16_m4:
**   vl4re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   vs4r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   ret
*/
void mov_vf16_m4 (float16_t *in, float16_t *out)
{
  vfloat16m4_t v = *(vfloat16m4_t *)in;
  * (vfloat16m4_t *) out = v;
}

/*
** mov_vf16_m8:
**   vl8re16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   vs8r\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)
**   ret
*/
void mov_vf16_m8 (float16_t *in, float16_t *out)
{
  vfloat16m8_t v = *(vfloat16m8_t *)in;
  * (vfloat16m8_t *) out = v;
}
