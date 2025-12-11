/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -march=la464 -mno-strict-align" } */
/* { dg-final { scan-assembler-times "xvst" 2 } } */
/* { dg-final { scan-assembler-times "\tvst" 1 } } */
/* { dg-final { scan-assembler-times "st\\.d|stptr\\.d" 1 } } */
/* { dg-final { scan-assembler-times "st\\.w|stptr\\.w" 1 } } */
/* { dg-final { scan-assembler-times "st\\.h" 1 } } */
/* { dg-final { scan-assembler-times "st\\.b" 1 } } */

extern char a[], b[];
void test() { __builtin_memcpy(a, b, 95); }
