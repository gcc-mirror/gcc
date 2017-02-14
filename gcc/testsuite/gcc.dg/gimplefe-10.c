/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE() bar(int a, int b, int c)
{
  a = 1;
  b = a + 1;
  c = b * 4;
  return b;
}

void __GIMPLE() foo()
{
  int a;
  int b;
  int c;
  b = bar(a, b, c);
}
