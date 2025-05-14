/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1 -mno-avx512f -Wno-psabi" } */
/* { dg-final { scan-assembler-not "%zmm" } } */

#include "avx10_1-2.c"
