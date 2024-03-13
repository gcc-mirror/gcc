/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -ffast-math -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_CONVERT (fp16, int64_t, _Float16, 1)
DEF_CONVERT (fp16, int64_t, _Float16, 2)
DEF_CONVERT (fp16, int64_t, _Float16, 4)
DEF_CONVERT (fp16, int64_t, _Float16, 8)
DEF_CONVERT (fp16, int64_t, _Float16, 16)
DEF_CONVERT (fp16, int64_t, _Float16, 32)
DEF_CONVERT (fp16, int64_t, _Float16, 64)
DEF_CONVERT (fp16, int64_t, _Float16, 128)
DEF_CONVERT (fp16, int64_t, _Float16, 256)
DEF_CONVERT (fp16, int64_t, _Float16, 512)
DEF_CONVERT (fp16, int64_t, _Float16, 1024)
DEF_CONVERT (fp16, int64_t, _Float16, 2048)

DEF_CONVERT (fp16, uint64_t, _Float16, 1)
DEF_CONVERT (fp16, uint64_t, _Float16, 2)
DEF_CONVERT (fp16, uint64_t, _Float16, 4)
DEF_CONVERT (fp16, uint64_t, _Float16, 8)
DEF_CONVERT (fp16, uint64_t, _Float16, 16)
DEF_CONVERT (fp16, uint64_t, _Float16, 32)
DEF_CONVERT (fp16, uint64_t, _Float16, 64)
DEF_CONVERT (fp16, uint64_t, _Float16, 128)
DEF_CONVERT (fp16, uint64_t, _Float16, 256)
DEF_CONVERT (fp16, uint64_t, _Float16, 512)
DEF_CONVERT (fp16, uint64_t, _Float16, 1024)
DEF_CONVERT (fp16, uint64_t, _Float16, 2048)

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
/* { dg-final { scan-assembler-times {vfncvt\.f\.x\.w\s+v[0-9]+,\s*v[0-9]+} 15 } } */
/* { dg-final { scan-assembler-times {vfncvt\.f\.xu\.w\s+v[0-9]+,\s*v[0-9]+} 15 } } */
/* { dg-final { scan-assembler-times {vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+} 30 } } */
