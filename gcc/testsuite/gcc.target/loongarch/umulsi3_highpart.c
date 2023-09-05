/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int DI __attribute__((mode(DI)));
typedef unsigned int SI __attribute__((mode(SI)));

SI
f (SI x, SI y)
{
  return ((DI) x * y) >> 32;
}

/* { dg-final { scan-assembler "mulh\\.wu" } } */
/* { dg-final { scan-assembler-not "slli\\.w" } } */
