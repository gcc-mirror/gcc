/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1 -mno-avx512f" } */
/* { dg-warning "'-mno-evex512' or '-mno-avx512XXX' cannot disable AVX10 instructions when AVX10.1-512 is available" "" { target *-*-* } 0 } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%zmm" } } */

#include "avx10_1-2.c"
