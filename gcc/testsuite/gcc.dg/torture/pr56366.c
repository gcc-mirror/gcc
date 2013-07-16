/* { dg-do compile } */

int a, *c, d;
unsigned short b;
short e;

void f(void)
{
  for(;; d++)
    {
      for(a = -9; a < 63; a++)
	for(d = 0; d < 9; d++)
	  b -= --e;

      a = b & *c;
    }
}
