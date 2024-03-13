/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_SCALAR_INDEX (vec_set, v1si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v2si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v4si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v8si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v16si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v32si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v64si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v128si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v256si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v512si, int32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v1024si, int32_t)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vslideup\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[atx][0-9]+} 10 } } */
