/* { dg-do compile } */ /* PR59617 */
/* { dg-options "-O3 -mavx512f -fdump-tree-vect-details -mtune=knl" } */

#include "avx512f-gather-1.c"

/* { dg-final { scan-assembler-not "gather\[^\n\]*ymm\[^\n\]*ymm" } } */
/* { dg-final { scan-assembler-not "gather\[^\n\]*xmm\[^\n\]*ymm" } } */
/* { dg-final { scan-assembler-not "gather\[^\n\]*ymm\[^\n\]*xmm" } } */
/* { dg-final { scan-assembler-not "gather\[^\n\]*xmm\[^\n\]*xmm" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 16 "vect" } } */
