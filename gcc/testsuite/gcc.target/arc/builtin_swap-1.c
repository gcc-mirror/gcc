/* { dg-do compile } */
/* { dg-options "-O2 -mswap" } */

int foo(int x)
{
  return __builtin_arc_swap (x);
}

/* { dg-final { scan-assembler "swap\\s+r0,r0" } } */
