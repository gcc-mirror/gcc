/* { dg-do compile } */
/* { dg-options "-O2" } */

short neg(short x)
{
  return -x;
}
/* { dg-final { scan-assembler "not r2 | add r2,#1" } } */
