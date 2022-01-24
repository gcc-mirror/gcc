/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(int x)
{
  return __builtin_parity(x);
}

/* { dg-final { scan-assembler "ONES" } } */
