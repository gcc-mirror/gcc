/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=em -mswap" } */

unsigned int foo(unsigned int x)
{
  return x >> 16;
}

/* { dg-final { scan-assembler "lsr16\\s+r0,r0" } } */

