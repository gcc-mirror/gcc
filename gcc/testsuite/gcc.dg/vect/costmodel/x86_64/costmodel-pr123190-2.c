/* { dg-do compile } */
/* { dg-additional-options "-O2 -mavx2 -mno-avx512f" } */

#include "costmodel-pr123190-1.c"

/* { dg-final { scan-tree-dump "optimized: loop vectorized using 32" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: epilogue loop vectorized using 16 byte vectors and unroll factor 1" "vect" } } */
