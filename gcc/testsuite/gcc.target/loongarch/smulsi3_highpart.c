/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand-all" } */

typedef unsigned int DI __attribute__((mode(DI)));
typedef unsigned int SI __attribute__((mode(SI)));

SI
f (SI x, SI y)
{
  return ((DI) x * y) >> 32;
}

/* { dg-final { scan-rtl-dump "highparttmp" "expand" } } */
/* { dg-final { scan-assembler "mulh\\.w" } } */
/* { dg-final { scan-assembler-not "slli\\.w" } } */
