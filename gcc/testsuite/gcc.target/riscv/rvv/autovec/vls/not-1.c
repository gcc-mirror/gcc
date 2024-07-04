/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (not, 1, int8_t, ~)
DEF_OP_V (not, 2, int8_t, ~)
DEF_OP_V (not, 4, int8_t, ~)
DEF_OP_V (not, 8, int8_t, ~)
DEF_OP_V (not, 16, int8_t, ~)
DEF_OP_V (not, 32, int8_t, ~)
DEF_OP_V (not, 64, int8_t, ~)
DEF_OP_V (not, 128, int8_t, ~)
DEF_OP_V (not, 256, int8_t, ~)
DEF_OP_V (not, 512, int8_t, ~)
DEF_OP_V (not, 1024, int8_t, ~)
DEF_OP_V (not, 2048, int8_t, ~)
DEF_OP_V (not, 4096, int8_t, ~)

DEF_OP_V (not, 1, int16_t, ~)
DEF_OP_V (not, 2, int16_t, ~)
DEF_OP_V (not, 4, int16_t, ~)
DEF_OP_V (not, 8, int16_t, ~)
DEF_OP_V (not, 16, int16_t, ~)
DEF_OP_V (not, 32, int16_t, ~)
DEF_OP_V (not, 64, int16_t, ~)
DEF_OP_V (not, 128, int16_t, ~)
DEF_OP_V (not, 256, int16_t, ~)
DEF_OP_V (not, 512, int16_t, ~)
DEF_OP_V (not, 1024, int16_t, ~)
DEF_OP_V (not, 2048, int16_t, ~)

DEF_OP_V (not, 1, int32_t, ~)
DEF_OP_V (not, 2, int32_t, ~)
DEF_OP_V (not, 4, int32_t, ~)
DEF_OP_V (not, 8, int32_t, ~)
DEF_OP_V (not, 16, int32_t, ~)
DEF_OP_V (not, 32, int32_t, ~)
DEF_OP_V (not, 64, int32_t, ~)
DEF_OP_V (not, 128, int32_t, ~)
DEF_OP_V (not, 256, int32_t, ~)
DEF_OP_V (not, 512, int32_t, ~)
DEF_OP_V (not, 1024, int32_t, ~)

DEF_OP_V (not, 1, int64_t, ~)
DEF_OP_V (not, 2, int64_t, ~)
DEF_OP_V (not, 4, int64_t, ~)
DEF_OP_V (not, 8, int64_t, ~)
DEF_OP_V (not, 16, int64_t, ~)
DEF_OP_V (not, 32, int64_t, ~)
DEF_OP_V (not, 64, int64_t, ~)
DEF_OP_V (not, 128, int64_t, ~)
DEF_OP_V (not, 256, int64_t, ~)
DEF_OP_V (not, 512, int64_t, ~)

/* { dg-final { scan-assembler-times {vnot\.v\s+v[0-9]+,\s*v[0-9]+} 42 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
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
