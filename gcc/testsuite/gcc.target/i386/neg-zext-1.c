/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

long long foo(unsigned int x) { return -(long long)x; }

/* { dg-final { scan-assembler "sbb" } } */
/* { dg-final { scan-assembler-not "adc" } } */
