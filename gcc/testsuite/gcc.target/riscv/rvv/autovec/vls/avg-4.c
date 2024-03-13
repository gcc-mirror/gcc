/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_AVG_CEIL (int8_t, int16_t, 4)
DEF_AVG_CEIL (int8_t, int16_t, 8)
DEF_AVG_CEIL (int8_t, int16_t, 16)
DEF_AVG_CEIL (int8_t, int16_t, 32)
DEF_AVG_CEIL (int8_t, int16_t, 64)
DEF_AVG_CEIL (int8_t, int16_t, 128)
DEF_AVG_CEIL (int8_t, int16_t, 256)
DEF_AVG_CEIL (int8_t, int16_t, 512)
DEF_AVG_CEIL (int8_t, int16_t, 1024)
DEF_AVG_CEIL (int8_t, int16_t, 2048)

DEF_AVG_CEIL (uint8_t, uint16_t, 4)
DEF_AVG_CEIL (uint8_t, uint16_t, 8)
DEF_AVG_CEIL (uint8_t, uint16_t, 16)
DEF_AVG_CEIL (uint8_t, uint16_t, 32)
DEF_AVG_CEIL (uint8_t, uint16_t, 64)
DEF_AVG_CEIL (uint8_t, uint16_t, 128)
DEF_AVG_CEIL (uint8_t, uint16_t, 256)
DEF_AVG_CEIL (uint8_t, uint16_t, 512)
DEF_AVG_CEIL (uint8_t, uint16_t, 1024)
DEF_AVG_CEIL (uint8_t, uint16_t, 2048)

/* { dg-final { scan-assembler-times {vwadd\.vv} 10 } } */
/* { dg-final { scan-assembler-times {csrwi\s*vxrm,\s*0} 10 } } */
/* { dg-final { scan-assembler-times {vnsra\.wi} 10 } } */
/* { dg-final { scan-assembler-times {vaaddu\.vv} 10 } } */
/* { dg-final { scan-assembler-times {vadd\.vi} 10 } } */
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
