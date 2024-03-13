/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_OP_WVV (cond_wmul, 4, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 8, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 16, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 32, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 64, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 128, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 256, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 512, qi, hi, int16_t, *)
DEF_COND_OP_WVV (cond_wmul, 1024, qi, hi, int16_t, *)

DEF_COND_OP_WVV (cond_wmul, 4, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 8, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 16, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 32, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 64, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 128, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 256, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 512, hi, si, int32_t, *)
DEF_COND_OP_WVV (cond_wmul, 1024, hi, si, int32_t, *)

DEF_COND_OP_WVV (cond_wmul, 4, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 8, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 16, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 32, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 64, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 128, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 256, si, di, int64_t, *)
DEF_COND_OP_WVV (cond_wmul, 512, si, di, int64_t, *)

DEF_COND_OP_WVV (cond_wmul, 4, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 8, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 16, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 32, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 64, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 128, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 256, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 512, uqi, uhi, uint16_t, *)
DEF_COND_OP_WVV (cond_wmul, 1024, uqi, uhi, uint16_t, *)

DEF_COND_OP_WVV (cond_wmul, 4, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 8, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 16, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 32, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 64, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 128, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 256, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 512, uhi, usi, uint32_t, *)
DEF_COND_OP_WVV (cond_wmul, 1024, uhi, usi, uint32_t, *)

DEF_COND_OP_WVV (cond_wmul, 4, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 8, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 16, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 32, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 64, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 128, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 256, usi, udi, uint64_t, *)
DEF_COND_OP_WVV (cond_wmul, 512, usi, udi, uint64_t, *)

/* { dg-final { scan-assembler-times {vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 26 } } */
/* { dg-final { scan-assembler-times {vwmulu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 26 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {vmerge} } } */
/* { dg-final { scan-tree-dump-not "1,1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2,2" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4,4" "optimized" } } */
/* { dg-final { scan-tree-dump-not "16,16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "32,32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64,64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "128,128" "optimized" } } */
/* { dg-final { scan-tree-dump-not "256,256" "optimized" } } */
/* { dg-final { scan-tree-dump-not "512,512" "optimized" } } */
/* { dg-final { scan-tree-dump-not "1024,1024" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2048,2048" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4096,4096" "optimized" } } */
