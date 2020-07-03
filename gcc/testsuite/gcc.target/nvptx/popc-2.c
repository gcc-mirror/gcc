/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long foo(unsigned long x)
{
  return __builtin_popcountl(x);
}

/* { dg-final { scan-assembler-times "popc.b64" 1 } } */
/* { dg-final { scan-assembler-times "cvt.s64.s32" 1 } } */

