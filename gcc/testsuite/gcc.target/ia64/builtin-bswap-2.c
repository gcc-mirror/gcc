/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "mux1" } } */

int foo (int x)
{
  return __builtin_bswap32 (x);
}

