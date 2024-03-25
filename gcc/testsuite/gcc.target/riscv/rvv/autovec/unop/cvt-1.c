/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -ftree-vectorize -ffast-math -fno-vect-cost-model -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>

/*
** test_uint65_to_fp16:
**   ...
**   vfncvt\.f\.xu\.w\s+v[0-9]+,\s*v[0-9]+
**   ...
**   vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+
**   ...
*/
void
test_uint65_to_fp16 (uint64_t * __restrict a, _Float16 *b, unsigned n)
{
  for (unsigned i = 0; i < n; i++)
    b[i] = (_Float16) (a[i]);
}

