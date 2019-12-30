/* PR target/93078 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f -mprefer-vector-width=512 -masm=att" } */
/* { dg-final { scan-assembler "vrndscaleps\[ \t]\+\\\$12,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscaleps\[ \t]\+\\\$4,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscalepd\[ \t]\+\\\$12,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscalepd\[ \t]\+\\\$4,\[^\n\r]*%z" } } */

#include "sse4_1-pr93078.c"
