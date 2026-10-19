/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

/*
** shared_reductions:
**	faddp\ts[0-9]+, v[0-9]+\.2s
**	faddp\ts[0-9]+, v[0-9]+\.2s
**	(?:str|zip1|ins)\t[^\n]+
**	(?:str|zip1|ins)\t[^\n]+
**	(?:str|zip1|ins)\t[^\n]+
**	ret
*/
float32x2_t
shared_reductions (float32x2_t a, float32x2_t b,
		   volatile float *p, volatile float *q)
{
  float x = vaddv_f32 (a);
  float y = vaddv_f32 (b);
  *p = x;
  *q = y;
  return (float32x2_t) { x, y };
}
