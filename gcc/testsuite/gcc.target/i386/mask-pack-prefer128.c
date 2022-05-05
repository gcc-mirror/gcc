/* { dg-do compile } */
/* { dg-options "-march=skylake-avx512 -O3 -fopenmp-simd -fdump-tree-vect-details -mprefer-vector-width=128" } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 10 "vect" } } */
/* { dg-final { scan-assembler-not "maskmov" } } */

#include "mask-pack.c"
