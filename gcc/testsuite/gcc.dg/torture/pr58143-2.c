/* { dg-do run } */
/* { dg-additional-options "-fstrict-overflow" } */

int a, b, d, e, f, *g, h, i;
volatile int c;

char foo (unsigned char p)
{
  return p + 1;
}

int bar () 
{
  for (h = 0; h < 3; h = foo (h))
    {
      c;
      for (f = 0; f < 1; f++)
	{
	  i = a && 0 < -__INT_MAX__ - h ? 0 : 1;
	  if (e)
	    for (; d;)
	      b = 0;
	  else
	    g = 0;
	}
    }
  return 0;
}

int main ()
{
  bar ();
  return 0;
}
