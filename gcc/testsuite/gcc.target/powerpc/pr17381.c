/* PR target/17381 - Unnecessary register move for float extend */
/* { dg-do compile } */
/* { dg-options "-O2" } */

double d;
float test1(float fParm)
{
  d = fParm + 1.0;
  return fParm + 1.0f;
}
/* { dg-final { scan-assembler-times "fmr" 1 } } */
