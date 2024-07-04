/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V_CVT (llroundf16, 1, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 2, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 4, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 8, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 16, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 32, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 64, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 128, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 256, _Float16, int64_t, __builtin_llroundf16)
DEF_OP_V_CVT (llroundf16, 512, _Float16, int64_t, __builtin_llroundf16)

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
/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v\s+v[0-9]+,\s*v[0-9]+} 9 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 9 } } */
