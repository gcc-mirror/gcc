/* { dg-do compile } */
/* { dg-options "-O2 -mnorm" } */

int foo(int x)
{
  return __builtin_arc_norm (x);
}

/* { dg-final { scan-assembler "norm\\s+r0,r0" } } */
