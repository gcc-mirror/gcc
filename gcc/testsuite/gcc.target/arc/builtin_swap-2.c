/* { dg-do compile } */
/* { dg-options "-O2 -mswap" } */

int foo()
{
  return __builtin_arc_swap (0x12345678);
}

/* { dg-final { scan-assembler-not "swap\\s+r" } } */
