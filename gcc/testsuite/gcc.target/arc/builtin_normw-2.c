/* { dg-do compile } */
/* { dg-options "-O2 -mnorm" } */

int foo()
{
  return __builtin_arc_normw (255);
}

/* { dg-final { scan-assembler-not "normh\\s+r" } } */
/* { dg-final { scan-assembler "mov_s\\s+r0,7" } } */
