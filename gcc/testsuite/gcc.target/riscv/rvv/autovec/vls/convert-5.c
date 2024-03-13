/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_CONVERT (fwcvt, int8_t, _Float16, 4)
DEF_CONVERT (fwcvt, int8_t, _Float16, 16)
DEF_CONVERT (fwcvt, int8_t, _Float16, 32)
DEF_CONVERT (fwcvt, int8_t, _Float16, 64)
DEF_CONVERT (fwcvt, int8_t, _Float16, 128)
DEF_CONVERT (fwcvt, int8_t, _Float16, 256)
DEF_CONVERT (fwcvt, int8_t, _Float16, 512)
DEF_CONVERT (fwcvt, int8_t, _Float16, 1024)
DEF_CONVERT (fwcvt, int8_t, _Float16, 2048)
DEF_CONVERT (fwcvt, int8_t, _Float16, 4096)

DEF_CONVERT (fwcvt, uint8_t, _Float16, 4)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 16)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 32)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 64)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 128)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 256)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 512)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 1024)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 2048)
DEF_CONVERT (fwcvt, uint8_t, _Float16, 4096)

DEF_CONVERT (fwcvt, int16_t, float, 4)
DEF_CONVERT (fwcvt, int16_t, float, 16)
DEF_CONVERT (fwcvt, int16_t, float, 32)
DEF_CONVERT (fwcvt, int16_t, float, 64)
DEF_CONVERT (fwcvt, int16_t, float, 128)
DEF_CONVERT (fwcvt, int16_t, float, 256)
DEF_CONVERT (fwcvt, int16_t, float, 512)
DEF_CONVERT (fwcvt, int16_t, float, 1024)
DEF_CONVERT (fwcvt, int16_t, float, 2048)

DEF_CONVERT (fwcvt, uint16_t, float, 4)
DEF_CONVERT (fwcvt, uint16_t, float, 16)
DEF_CONVERT (fwcvt, uint16_t, float, 32)
DEF_CONVERT (fwcvt, uint16_t, float, 64)
DEF_CONVERT (fwcvt, uint16_t, float, 128)
DEF_CONVERT (fwcvt, uint16_t, float, 256)
DEF_CONVERT (fwcvt, uint16_t, float, 512)
DEF_CONVERT (fwcvt, uint16_t, float, 1024)
DEF_CONVERT (fwcvt, uint16_t, float, 2048)

DEF_CONVERT (fwcvt, int32_t, double, 4)
DEF_CONVERT (fwcvt, int32_t, double, 16)
DEF_CONVERT (fwcvt, int32_t, double, 32)
DEF_CONVERT (fwcvt, int32_t, double, 64)
DEF_CONVERT (fwcvt, int32_t, double, 128)
DEF_CONVERT (fwcvt, int32_t, double, 256)
DEF_CONVERT (fwcvt, int32_t, double, 512)
DEF_CONVERT (fwcvt, int32_t, double, 1024)

DEF_CONVERT (fwcvt, uint32_t, double, 4)
DEF_CONVERT (fwcvt, uint32_t, double, 16)
DEF_CONVERT (fwcvt, uint32_t, double, 32)
DEF_CONVERT (fwcvt, uint32_t, double, 64)
DEF_CONVERT (fwcvt, uint32_t, double, 128)
DEF_CONVERT (fwcvt, uint32_t, double, 256)
DEF_CONVERT (fwcvt, uint32_t, double, 512)
DEF_CONVERT (fwcvt, uint32_t, double, 1024)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v} 30 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.xu\.v} 30 } } */
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
