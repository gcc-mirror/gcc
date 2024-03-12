/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned short foo(unsigned short x)
{
  return __builtin_bswap16 (x);
}

/* { dg-final { scan-assembler "swpb r2" } } */
