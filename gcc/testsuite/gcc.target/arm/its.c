/* { dg-do compile } */
/* { dg-options "-O2" }  */
int test (int a, int b)
{
  int r;
  if (a > 10)
    {
      r = a - b;
      r += 10;
    }
  else
    {
      r = b - a;
      r -= 7;
    }
  if (r > 0)
    r -= 3;
  return r;
}
/* { dg-final { scan-assembler-times "\tit" 2 { target arm_thumb2 } } } */
