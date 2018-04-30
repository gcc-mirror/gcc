/* Regression test for PR/80725.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=zEC12" } */

int a, e;
const char b;
char c;
const int d;
void bar (short);

void
foo (int x, int y)
{
  long f = d;
  short g = 0;
  while (e)
    while (a < x)
      {
	if (y)
	  goto *d;
	g = b | b + g;
	bar (g);
	c = (char) (long) foo;
      }
}
