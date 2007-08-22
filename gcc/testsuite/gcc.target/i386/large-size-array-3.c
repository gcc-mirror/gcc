/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mcmodel=medium" } */
/* { dg-final { scan-assembler "8589934588" } } */
int bigarray[2147483647];
