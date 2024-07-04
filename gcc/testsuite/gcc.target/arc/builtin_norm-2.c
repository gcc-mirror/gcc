/* { dg-do compile } */
/* { dg-options "-O2 -mnorm" } */

int foo()
{
  return __builtin_arc_norm (255);
}

/* { dg-final { scan-assembler-not "norm\\s+r0,r0" } } */
/* { dg-final { scan-assembler "mov_s\\s+r0,23" } } */
