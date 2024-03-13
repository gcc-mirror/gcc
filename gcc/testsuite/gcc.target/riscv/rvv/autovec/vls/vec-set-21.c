/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_SCALAR_INDEX (vec_set, v1sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v2sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v4sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v8sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v16sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v32sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v64sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v128sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v256sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v512sf, float)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v1024sf, float)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vslideup\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[atx][0-9]+} 10 } } */
