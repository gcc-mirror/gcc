/* PR target/127177 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx -fno-trapping-math -fdump-tree-vect-details" } */

/* Without -ftrapping-math the loops should be vectorized.  */

#include "pr127177-1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 5 "vect" } } */
