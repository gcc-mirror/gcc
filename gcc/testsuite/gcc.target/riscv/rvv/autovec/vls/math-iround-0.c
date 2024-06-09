/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V_CVT (iroundf, 1, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 2, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 4, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 8, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 16, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 32, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 64, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 128, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 256, float, int, __builtin_iroundf)
DEF_OP_V_CVT (iroundf, 512, float, int, __builtin_iroundf)

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
/* { dg-final { scan-assembler-times {vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 9 } } */
