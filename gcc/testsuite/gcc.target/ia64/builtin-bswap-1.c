/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "mux1" } } */

long foo (long x)
{
  return __builtin_bswap64 (x);
}

