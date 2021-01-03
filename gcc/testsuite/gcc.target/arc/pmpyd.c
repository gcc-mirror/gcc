/* { dg-do assemble } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=hs38 -Os -EB" } */

/* This example is found during big-endian build.  The compiler is
   matching mpydu.hi r12,r13,r3 as a predicated instruction, which is
   incorrect.  The error is due to different predicates between the
   output operand and the first operand of the instruction.  */
unsigned int test(unsigned int x, unsigned long long y)
{
  y /= 0x20000000;
  if (x > 1)
    y *= x;
  return y;
}
