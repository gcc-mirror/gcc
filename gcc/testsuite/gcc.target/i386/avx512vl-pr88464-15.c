/* PR tree-optimization/88464 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512vl -mprefer-vector-width=128 -fno-vect-cost-model -mtune=skylake-avx512 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 16 byte vectors" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */

#include "avx512f-pr88464-7.c"
