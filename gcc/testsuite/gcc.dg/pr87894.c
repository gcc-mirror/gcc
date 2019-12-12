/* { dg-do compile } */
/* { dg-options "-Ofast" } */

int a, b, c, d;
double e;

void f(double g[][1])
{
  for (;;)
    {
      double h;
      for (; b < c; b++)
	{
	  if (b >= 0)
	    ;
	  else if (d)
	    h = 2.0;
	  else
	    h = 0.0;
	  if (e)
	    g[a][b] = 0.0;
	  g[a][b] = h;
	}
    }
}

