/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1-256 -mavx512f" } */
/* { dg-warning "Vector size conflicts between AVX10.1 and AVX512, using 512 as max vector size" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%zmm" } } */

#include "avx10_1-2.c"
