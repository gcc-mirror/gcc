/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1 -mevex512 -Wno-psabi" } */
/* { dg-warning "Using '-mevex512' without any AVX512 features enabled together with AVX10.1 only will not enable any AVX512 or AVX10.1-512 features, using 256 as max vector size" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-not "%zmm" } } */

#include "avx10_1-2.c"
