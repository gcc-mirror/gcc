/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_SCALAR_INDEX (vec_set, v1usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v2usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v4usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v8usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v16usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v32usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v64usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v128usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v256usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v512usi, uint32_t)
DEF_VEC_SET_SCALAR_INDEX (vec_set, v1024usi, uint32_t)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vslideup\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[atx][0-9]+} 10 } } */
