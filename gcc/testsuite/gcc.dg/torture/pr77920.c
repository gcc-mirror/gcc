/* { dg-do compile } */

int a, b;
void fn1()
{
  int c;
  for (; b < 0;)
    {
	{
	  int d = 56, e = (b >> 3) - (d >> 3) > 0 ? (b >> 3) - (d >> 3)
	      : -((b >> 3) - (d >> 3));
	  c = 1 >= e;
	}
      if (c)
	a = 0;
    }
}
