/* PR rtl-optimization/45617 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int f1 (int x)
{
  return (x >> 23) > 12;
}
int f2 (int x)
{
  return x > ((13 << 23) - 1);
}
int f3 (int x)
{
  return (x >> 23) >= 12;
}
int f4 (int x)
{
  return x >= (12 << 23);
}

/* { dg-final { scan-assembler-not "sarl" } } */
