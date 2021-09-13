/* PR rtl-optimization/98334 */
/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fno-stack-protector" } */
/* { dg-final { scan-assembler-not "\taddl\t" } } */
/* { dg-final { scan-assembler-not "\tsubl\t" } } */
/* { dg-final { scan-assembler-not "\tleal\t" } } */

int
foo (int i, unsigned int n)
{
  int result = 0;
  while (n > 0)
    {
      result += i;
      n -= 1;
    }
  return result;
}

int
bar (int x, int y)
{
  return (int) (y - 1U) * x + x;
}

int
baz (int x, int y)
{
  return (y - 1) * x + x;
}

int
qux (int x, int y)
{
  return x * (int) (y + 1U) - x;
}
