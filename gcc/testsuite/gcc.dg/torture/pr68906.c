/* { dg-do compile } */
/* { dg-options "-O3" } */

int a;
volatile int b;
short c, d;
int
fn1 ()
{
  int e;
  for (;;)
    {
      a = 3;
      if (c)
	continue;
      e = 0;
      for (; e > -30; e--)
	if (b)
	  {
	    int f = e;
	    return d;
	  }
    }
}
