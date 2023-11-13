/* { dg-do compile } */
/* { dg-options "-O2 -mswap" } */

int foo(int x)
{
  int t = __builtin_arc_swap (x);
  return __builtin_arc_swap (t);
}

/* { dg-final { scan-assembler-not "swap\\s+r" } } */
