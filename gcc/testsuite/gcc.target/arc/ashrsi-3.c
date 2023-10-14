/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=em" } */

int foo(int x, int y) { return y >> 1; }

/* { dg-final { scan-assembler-times "asr_s\\s+r0,r1" 1 } } */
/* { dg-final { scan-assembler "j_s\.d" } } */
