/* { dg-do compile } */
/* { dg-options "-march=r3900" } */
/* { dg-final { scan-assembler "\tmult\t\[^\n\]*,\[^\n\]*," } } */

NOMIPS16 int
f (int a, int b)
{
  return a * b;
}
