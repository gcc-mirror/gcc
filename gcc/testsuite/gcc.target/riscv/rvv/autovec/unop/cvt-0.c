/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -ftree-vectorize -ffast-math -fno-vect-cost-model -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

/*
** test_int65_to_fp16:
**   ...
**   vsetvli\s+[atx][0-9]+,\s*zero,\s*e32,\s*mf2,\s*ta,\s*ma
**   vfncvt\.f\.x\.w\s+v[0-9]+,\s*v[0-9]+
**   vsetvli\s+zero,\s*zero,\s*e16,\s*mf4,\s*ta,\s*ma
**   vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+
**   ...
*/
void
test_int65_to_fp16 (int64_t * __restrict a, _Float16 *b, unsigned n)
{
  for (unsigned i = 0; i < n; i++)
    b[i] = (_Float16) (a[i]);
}
