/* { dg-additional-options "-O2 -fno-schedule-insns" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "arm_sve.h"

/*
** foo:
**        ptrue   (p[0-7])\.d, all
**        pfalse  (p[0-7])\.b
**        ptrue   (p[0-7])\.s, all
**        trn1    (p[0-7])\.d, \2\.d, \3\.d
**        trn1    \2\.d, \1\.d, \3\.d
**        faddv   (h[0-31]), \4\, (z[0-31]).h
**        faddv   (h[0-31]), \2\, \6\.h
**        str     \5, [x0]
**        str     \7, [x0, 2]
**        ret
*/
void foo(svfloat16_t in, float16_t *dst) {
  const svbool_t pg_q0 = svdupq_n_b16(1, 0, 1, 0, 0, 0, 0, 0);
  const svbool_t pg_f0 = svdupq_n_b16(1, 0, 0, 0, 0, 0, 0, 0);
  dst[0] = svaddv_f16(pg_f0, in);
  dst[1] = svaddv_f16(pg_q0, in);
}

