/* { dg-do run } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-O2 -fopenmp-simd -mavx512vl" } */

#define SIMDLEN 8
#include "vect-simd-clone-avx512-1.c"
