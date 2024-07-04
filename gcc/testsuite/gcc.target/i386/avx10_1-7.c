/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1-512 -mavx512f" } */
/* { dg-final { scan-assembler "%zmm" } } */

#include "avx10_1-2.c"
