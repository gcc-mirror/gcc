/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (nearbyintf16, 1, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 2, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 4, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 8, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 16, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 32, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 64, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 128, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 256, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 512, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 1024, _Float16, __builtin_nearbyintf16)
DEF_OP_V (nearbyintf16, 2048, _Float16, __builtin_nearbyintf16)

DEF_OP_V (nearbyintf, 1, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 2, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 4, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 8, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 16, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 32, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 64, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 128, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 256, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 512, float, __builtin_nearbyintf)
DEF_OP_V (nearbyintf, 1024, float, __builtin_nearbyintf)

DEF_OP_V (nearbyint, 1, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 2, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 4, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 8, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 16, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 32, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 64, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 128, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 256, double, __builtin_nearbyint)
DEF_OP_V (nearbyint, 512, double, __builtin_nearbyint)

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
/* { dg-final { scan-assembler-times {vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0\.t} 30 } } */
/* { dg-final { scan-assembler-times {vfcvt\.f\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0\.t} 30 } } */
/* { dg-final { scan-assembler-times {frflags\s+[atx][0-9]+} 30 } } */
/* { dg-final { scan-assembler-times {fsflags\s+[atx][0-9]+} 30 } } */
