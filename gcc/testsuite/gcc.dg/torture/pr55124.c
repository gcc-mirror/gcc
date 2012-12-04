/* { dg-do compile } */

int a, b;
long c;

void f2(void)
{
  unsigned long k = 1;

  foo(b ? k = 0 : 0);

  b = ((c = b) ? (k ? : (c = 0)) : a) * c;
}

void f1(void)
{
  f2();

  a = b | c;
}
