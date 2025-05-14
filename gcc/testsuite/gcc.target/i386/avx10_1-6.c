/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512f -mno-avx10.1" } */
/* { dg-final { scan-assembler "%zmm" } } */

#include "avx10_1-2.c"
