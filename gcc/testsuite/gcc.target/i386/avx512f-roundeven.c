/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f -mprefer-vector-width=512 -masm=att -ftrapping-math" } */
/* { dg-final { scan-assembler "vrndscaleps\[ \t]\+\\\$8,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscalepd\[ \t]\+\\\$8,\[^\n\r]*%z" } } */

#include "sse4_1-roundeven.c"
