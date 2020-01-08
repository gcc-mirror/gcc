/* PR target/93078 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mno-avx2 -mprefer-vector-width=256 -masm=att" } */
/* { dg-final { scan-assembler "vroundps\[ \t]\+\\\$12,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundps\[ \t]\+\\\$4,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundpd\[ \t]\+\\\$12,\[^\n\r]*%y" } } */
/* { dg-final { scan-assembler "vroundpd\[ \t]\+\\\$4,\[^\n\r]*%y" } } */

#include "sse4_1-pr93078.c"
