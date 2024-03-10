/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long foo(unsigned long x)
{
  return __builtin_bswap32 (x);
}

/* { dg-final { scan-assembler "swpb" } } */
