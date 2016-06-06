/* { dg-do compile } */

unsigned a, b, c[1];
void __assert_fail() __attribute__((__noreturn__));
void fn1()
{
  int d;
  unsigned e;
  for (;;)
    {
      d = 0;
      for (; d <= 6; d++)
	c[d] || a ? 0 : __assert_fail();
      for (; e <= 5; e++)
	a = b;
    }
}
