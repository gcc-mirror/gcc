/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mno-avx10.1-512 -mavx512f" } */
/* { dg-warning "'-mno-avx10.1, -mno-avx10.1-256, -mno-avx10.1-512' cannot disable AVX512 instructions when '-mavx512XXX'" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%zmm" } } */

#include "avx10_1-2.c"
