/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_VEC_SET_IMM_INDEX (vec_set, v1uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v2uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v4uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v8uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v16uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v32uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v64uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v128uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v256uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v512uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v1024uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v2048uqi, uint8_t, 0)
DEF_VEC_SET_IMM_INDEX (vec_set, v4096uqi, uint8_t, 0)

DEF_VEC_SET_IMM_INDEX (vec_set, v2uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v4uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v8uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v16uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v32uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v64uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v128uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v256uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v512uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v1024uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v2048uqi, uint8_t, 1)
DEF_VEC_SET_IMM_INDEX (vec_set, v4096uqi, uint8_t, 1)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vmv\.s\.x\s+v[0-9]+,\s*[atx][0-9]+} 12 } } */
/* { dg-final { scan-assembler-times {vslideup\.vi\s+v[0-9]+,\s*v[0-9]+,\s*1} 12 } } */
