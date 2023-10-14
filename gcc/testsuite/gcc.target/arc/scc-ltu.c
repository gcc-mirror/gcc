/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=em" } */

unsigned int foo(unsigned int x, unsigned int y)
{
  return (x+y) < x;
}

/* { dg-final { scan-assembler "rlc\\s+r0,0" } } */
/* { dg-final { scan-assembler "add.f\\s+0,r0,r1" } } */
/* { dg-final { scan-assembler-not "mov_s\\s+r0,1" } } */
/* { dg-final { scan-assembler-not "mov\.hs\\s+r0,0" } } */
