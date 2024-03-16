/* { dg-do compile } */
/* { dg-options "-O2 -mnorm" } */

int foo(short x)
{
  return __builtin_arc_normw (x);
}

/* { dg-final { scan-assembler "normh\\s+r0, ?r0" } } */
