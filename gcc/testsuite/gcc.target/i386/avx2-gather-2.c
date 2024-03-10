/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-vect-details -march=skylake -mtune=haswell" } */

#include "avx2-gather-1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 16 "vect" } } */
/* { dg-final { scan-assembler "vpcmpeqd" } } */
