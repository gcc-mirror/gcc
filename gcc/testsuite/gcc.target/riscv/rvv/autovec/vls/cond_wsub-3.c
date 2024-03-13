/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_OP_WWV (cond_wsub, 4, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 8, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 16, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 32, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 64, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 128, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 256, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 512, qi, hi, int16_t, -)
DEF_COND_OP_WWV (cond_wsub, 1024, qi, hi, int16_t, -)

DEF_COND_OP_WWV (cond_wsub, 4, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 8, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 16, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 32, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 64, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 128, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 256, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 512, hi, si, int32_t, -)
DEF_COND_OP_WWV (cond_wsub, 1024, hi, si, int32_t, -)

DEF_COND_OP_WWV (cond_wsub, 4, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 8, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 16, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 32, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 64, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 128, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 256, si, di, int64_t, -)
DEF_COND_OP_WWV (cond_wsub, 512, si, di, int64_t, -)

DEF_COND_OP_WWV (cond_wsub, 4, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 8, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 16, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 32, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 64, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 128, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 256, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 512, uqi, uhi, uint16_t, -)
DEF_COND_OP_WWV (cond_wsub, 1024, uqi, uhi, uint16_t, -)

DEF_COND_OP_WWV (cond_wsub, 4, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 8, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 16, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 32, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 64, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 128, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 256, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 512, uhi, usi, uint32_t, -)
DEF_COND_OP_WWV (cond_wsub, 1024, uhi, usi, uint32_t, -)

DEF_COND_OP_WWV (cond_wsub, 4, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 8, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 16, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 32, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 64, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 128, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 256, usi, udi, uint64_t, -)
DEF_COND_OP_WWV (cond_wsub, 512, usi, udi, uint64_t, -)

/* { dg-final { scan-assembler-times {vwsub\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 26 } } */
/* { dg-final { scan-assembler-times {vwsubu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 26 } } */
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
