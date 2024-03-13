/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_SCALAR_INDEX (vec_set, v1qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v2qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v4qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v8qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v16qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v32qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v64qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v128qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v256qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v512qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v1024qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v2048qi, int8_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v4096qi, int8_t)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vslideup\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[axt][0-9]+} 12 } } */
