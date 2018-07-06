/* PR middle-end/82875 */

signed char a;
unsigned b;
long c, d;
long long e;

void
foo (void)
{
  short f = a = 6;
  while (0)
    while (a <= 7)
      {
	for (;;)
	  ;
	lab:
	  while (c <= 73)
	    ;
	e = 20;
	d ? (a %= c) * (e *= a ? f : b) : 0;
      }
  goto lab;
}
