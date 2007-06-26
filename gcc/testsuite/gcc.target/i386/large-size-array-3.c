/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-mcmodel=medium" } */
/* { dg-final { scan-assembler "8589934588" } } */
int bigarray[2147483647];
