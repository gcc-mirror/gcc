/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler "nor" } } */

int foo (int x, int y)
{
  return ~(x | y);
}
