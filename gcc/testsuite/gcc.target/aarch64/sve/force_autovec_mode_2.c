/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=scalable -mautovec-preference=asimd-only --param=aarch64-autovec-mode=VNx4QI -fvect-cost-model=unlimited -fdump-tree-vect-details" } */

/* The forced SVE mode takes priority over the Advanced SIMD preference,
   including when cost comparisons are disabled.  */
#include "force_autovec_mode_1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Choosing vector mode VNx4QI" 2 "vect" } } */
/* { dg-final { scan-assembler {\tld1b\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler-not {\t[su]unpk} } } */
