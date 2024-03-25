/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=em -mswap" } */

int foo(int x)
{
  return x << 16;
}

/* { dg-final { scan-assembler "lsl16\\s+r0,r0" } } */

