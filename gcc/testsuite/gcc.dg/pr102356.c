/* PR rtl-optimization/102356 */
/* { dg-do compile { target int32plus } } */
/* { dg-options "-O3 -g" } */

signed char a = 0;
unsigned char b = 9;
unsigned long long c = 0xF1FBFC17225F7A57ULL;
int d = 0x3A6667C6;

unsigned char
foo (unsigned int x)
{
  unsigned int *e = &x;
  if ((c /= ((0 * (*e *= b)) <= 0)))
    ;
  for (d = 9; d > 2; d -= 2)
    {
      c = -2;
      do
	if ((*e *= *e))
	  {
	    a = 4;
	    do
	      {
		a -= 3;
		if ((*e *= *e))
		  b = 9;
	      }
	    while (a > 2);
	  }
      while (c++);
    }
}
