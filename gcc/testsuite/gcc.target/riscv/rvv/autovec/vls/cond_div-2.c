/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_BINOP (cond_div, 4, v4hf, /)
DEF_COND_BINOP (cond_div, 8, v8hf, /)
DEF_COND_BINOP (cond_div, 16, v16hf, /)
DEF_COND_BINOP (cond_div, 32, v32hf, /)
DEF_COND_BINOP (cond_div, 64, v64hf, /)
DEF_COND_BINOP (cond_div, 128, v128hf, /)
DEF_COND_BINOP (cond_div, 256, v256hf, /)
DEF_COND_BINOP (cond_div, 512, v512hf, /)
DEF_COND_BINOP (cond_div, 1024, v1024hf, /)
DEF_COND_BINOP (cond_div, 2048, v2048hf, /)

DEF_COND_BINOP (cond_div, 4, v4sf, /)
DEF_COND_BINOP (cond_div, 8, v8sf, /)
DEF_COND_BINOP (cond_div, 16, v16sf, /)
DEF_COND_BINOP (cond_div, 32, v32sf, /)
DEF_COND_BINOP (cond_div, 64, v64sf, /)
DEF_COND_BINOP (cond_div, 128, v128sf, /)
DEF_COND_BINOP (cond_div, 256, v256sf, /)
DEF_COND_BINOP (cond_div, 512, v512sf, /)
DEF_COND_BINOP (cond_div, 1024, v1024sf, /)

DEF_COND_BINOP (cond_div, 4, v4df, /)
DEF_COND_BINOP (cond_div, 8, v8df, /)
DEF_COND_BINOP (cond_div, 16, v16df, /)
DEF_COND_BINOP (cond_div, 32, v32df, /)
DEF_COND_BINOP (cond_div, 64, v64df, /)
DEF_COND_BINOP (cond_div, 128, v128df, /)
DEF_COND_BINOP (cond_div, 256, v256df, /)
DEF_COND_BINOP (cond_div, 512, v512df, /)

/* { dg-final { scan-assembler-times {vfdiv\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 27 } } */
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
