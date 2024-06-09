/* { dg-do compile } */ /* PR59617 */
/* { dg-options "-O3 -mavx512f -march=sapphirerapids -fdump-tree-vect-details -mprefer-vector-width=512" } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

#include "avx512f-gather-1.c"

/* { dg-final { scan-assembler-not "gather\[^\n\]*ymm\[^\n\]*ymm" } } */
/* { dg-final { scan-assembler-not "gather\[^\n\]*xmm\[^\n\]*ymm" } } */
/* { dg-final { scan-assembler-not "gather\[^\n\]*ymm\[^\n\]*xmm" } } */
/* { dg-final { scan-assembler-not "gather\[^\n\]*xmm\[^\n\]*xmm" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 16 "vect" } } */
