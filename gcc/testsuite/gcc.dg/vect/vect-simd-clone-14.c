/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd -fcommon" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "vect-simd-clone-11.c"

