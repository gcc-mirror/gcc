/* VEC_PERM_EXPR merges with a result wider than the inputs.  */
/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <arm_sve.h>

/* svld1rq is c = VEC_PERM <a, a> from a 128-bit vector; d = <c, c>.  */

svint32_t
f1 (int32_t *p)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  return svrev_s32 (c);
}

svint32_t
f2 (int32_t *p)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  return svzip1_s32 (c, c);
}

svint32_t
f3 (int32_t *p)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  return svuzp2_s32 (c, c);
}

/* d = <x, c>.  */

svint32_t
f4 (int32_t *p, svint32_t x)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  return svzip1_s32 (x, c);
}

svint32_t
f5 (int32_t *p, svint32_t x)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  return svuzp1_s32 (x, c);
}

/* d = <c, x>.  */

svint32_t
f6 (int32_t *p, svint32_t x)
{
  svint32_t c = svld1rq_s32 (svptrue_b8 (), p);
  return svtrn2_s32 (c, x);
}
