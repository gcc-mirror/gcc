/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mno-avx2 -mprefer-vector-width=256 -masm=att -ftrapping-math" } */
/* { dg-final { scan-assembler "vroundps\[ \t]\+\\\$8,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundpd\[ \t]\+\\\$8,\[^\n\r]*%y" } } */

#include "sse4_1-roundeven.c"
