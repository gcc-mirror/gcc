/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1-512 -mavx512f -mno-evex512" } */
/* { dg-warning "'-mno-evex512' or '-mno-avx512XXX' cannot disable AVX10 instructions when AVX10.1-512 is available" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%zmm" } } */

#include "avx10_1-2.c"
