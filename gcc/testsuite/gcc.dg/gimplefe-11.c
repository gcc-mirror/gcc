/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE() bar(int a, int b, int c)
{
  a = 1;
  b = a + 1;
  c = b * 4;
  return;
}

void __GIMPLE() foo()
{
  int a;
  int b;
  int c;
  bar(a, b, c);
}
