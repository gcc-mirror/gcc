/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512er" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "avx512er-vrcp28ps-3.c"

/* { dg-final { scan-assembler-times "vrcp28ps\[^\n\r\]*zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
