/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int foo(unsigned int x)
{
  return __builtin_popcount(x);
}

/* { dg-final { scan-assembler-times "popc.b32" 1 } } */
