/* PR target/127055 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mno-avx2 -mprefer-vector-width=256 -masm=att -ftrapping-math" } */
/* { dg-final { scan-assembler "vroundps\[ \t]\+\\\$11,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundps\[ \t]\+\\\$10,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundps\[ \t]\+\\\$9,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundpd\[ \t]\+\\\$11,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundpd\[ \t]\+\\\$10,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundpd\[ \t]\+\\\$9,\[^\n\r]*%y" } } */

#include "sse4_1-pr127055.c"
