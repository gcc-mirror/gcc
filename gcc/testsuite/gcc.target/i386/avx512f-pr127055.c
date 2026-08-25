/* PR target/127055 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f -mprefer-vector-width=512 -masm=att -ftrapping-math" } */
/* { dg-final { scan-assembler "vrndscaleps\[ \t]\+\\\$11,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscaleps\[ \t]\+\\\$10,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscaleps\[ \t]\+\\\$9,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscalepd\[ \t]\+\\\$11,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscalepd\[ \t]\+\\\$10,\[^\n\r]*%z" } } */
/* { dg-final { scan-assembler "vrndscalepd\[ \t]\+\\\$9,\[^\n\r]*%z" } } */

#include "sse4_1-pr127055.c"
