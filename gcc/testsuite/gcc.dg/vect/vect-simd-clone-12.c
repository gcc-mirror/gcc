/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-additional-sources vect-simd-clone-12a.c } */

#include "vect-simd-clone-10.c"

/* { dg-final { cleanup-tree-dump "vect" } } */
