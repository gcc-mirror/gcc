/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=em" } */

int foo(int x)
{
  return (x<<27)>>27;
}

/* { dg-final { scan-assembler "msk_s\\s+r0,r0,4" } } */
/* { dg-final { scan-assembler "xor\\s+r0,r0,16" } } */
/* { dg-final { scan-assembler "sub\\s+r0,r0,16" } } */
/* { dg-final { scan-assembler-not "lp\\s+2f" } } */
