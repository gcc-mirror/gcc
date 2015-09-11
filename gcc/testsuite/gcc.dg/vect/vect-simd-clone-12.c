/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-additional-sources vect-simd-clone-12a.c } */

#include "vect-simd-clone-10.c"

