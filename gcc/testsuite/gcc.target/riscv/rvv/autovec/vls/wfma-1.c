/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_FMA_WVV (wfma, 4, int8_t, int16_t)
DEF_FMA_WVV (wfma, 8, int8_t, int16_t)
DEF_FMA_WVV (wfma, 16, int8_t, int16_t)
DEF_FMA_WVV (wfma, 32, int8_t, int16_t)
DEF_FMA_WVV (wfma, 64, int8_t, int16_t)
DEF_FMA_WVV (wfma, 128, int8_t, int16_t)
DEF_FMA_WVV (wfma, 256, int8_t, int16_t)
DEF_FMA_WVV (wfma, 512, int8_t, int16_t)
DEF_FMA_WVV (wfma, 1024, int8_t, int16_t)
DEF_FMA_WVV (wfma, 2048, int8_t, int16_t)

DEF_FMA_WVV (wfma, 4, int16_t, int32_t)
DEF_FMA_WVV (wfma, 8, int16_t, int32_t)
DEF_FMA_WVV (wfma, 16, int16_t, int32_t)
DEF_FMA_WVV (wfma, 32, int16_t, int32_t)
DEF_FMA_WVV (wfma, 64, int16_t, int32_t)
DEF_FMA_WVV (wfma, 128, int16_t, int32_t)
DEF_FMA_WVV (wfma, 256, int16_t, int32_t)
DEF_FMA_WVV (wfma, 512, int16_t, int32_t)
DEF_FMA_WVV (wfma, 1024, int16_t, int32_t)

DEF_FMA_WVV (wfma, 4, int32_t, int64_t)
DEF_FMA_WVV (wfma, 8, int32_t, int64_t)
DEF_FMA_WVV (wfma, 16, int32_t, int64_t)
DEF_FMA_WVV (wfma, 32, int32_t, int64_t)
DEF_FMA_WVV (wfma, 64, int32_t, int64_t)
DEF_FMA_WVV (wfma, 128, int32_t, int64_t)
DEF_FMA_WVV (wfma, 256, int32_t, int64_t)
DEF_FMA_WVV (wfma, 512, int32_t, int64_t)

DEF_FMA_WVV (wfma, 4, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 8, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 16, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 32, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 64, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 128, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 256, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 512, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 1024, uint8_t, uint16_t)
DEF_FMA_WVV (wfma, 2048, uint8_t, uint16_t)

DEF_FMA_WVV (wfma, 4, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 8, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 16, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 32, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 64, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 128, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 256, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 512, uint16_t, uint32_t)
DEF_FMA_WVV (wfma, 1024, uint16_t, uint32_t)

DEF_FMA_WVV (wfma, 4, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 8, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 16, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 32, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 64, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 128, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 256, uint32_t, uint64_t)
DEF_FMA_WVV (wfma, 512, uint32_t, uint64_t)

/* { dg-final { scan-assembler-times {vwmacc\.vv} 27 } } */
/* { dg-final { scan-assembler-times {vwmaccu\.vv} 27 } } */
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
