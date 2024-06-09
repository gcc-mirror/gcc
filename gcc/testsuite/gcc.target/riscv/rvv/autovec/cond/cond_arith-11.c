/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include "cond_arith-1.c"

/* { dg-final { scan-tree-dump-times "\.COND_LEN_DIV" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MOD" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_RDIV" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_ADD" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_SUB" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_MUL" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_ADD" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_SUB" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_MUL" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_SUB" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MUL" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_SUB" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MUL" 3 "optimized" } } */
/* { dg-final { scan-assembler-times {vadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 8 } } */
/* { dg-final { scan-assembler-times {vsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 8 } } */
/* { dg-final { scan-assembler-times {vmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 8 } } */
/* { dg-final { scan-assembler-times {vdivu?\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 8 } } */
/* { dg-final { scan-assembler-times {vrem\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vremu\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfdiv\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
