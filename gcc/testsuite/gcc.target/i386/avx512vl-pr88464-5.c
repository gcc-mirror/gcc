/* PR tree-optimization/88464 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512vl -mprefer-vector-width=256 -mtune=skylake-avx512 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 32 byte vectors" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */

#include "avx512f-pr88464-3.c"
