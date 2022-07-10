/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "slli" } } */

int
div(int a, int b)
{
  return a / b;
}
