/* Since this uses dg-additional-sources, need to specify `dg-do run` instead of the default. */
/* { dg-do run } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */
/* { dg-additional-sources vect-simd-clone-12a.c } */

#include "vect-simd-clone-10.c"

