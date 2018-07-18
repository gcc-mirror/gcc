/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE() bar()
{
  int a;
  a = a + 1;
  return a;
}

void __GIMPLE() foo()
{
  int b;
  b = bar();
}
