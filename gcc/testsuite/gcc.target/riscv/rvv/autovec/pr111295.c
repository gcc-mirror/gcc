/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -Ofast -ftree-vectorize -mrvv-vector-bits=scalable -Wno-implicit-function-declaration" } */

#include <stdbool.h>
int a, b, c, e, f, g, h, i, j, k;
long l;
int q ()
{
  int r ();
  char *o, *d;
  _Bool p = f;
  while (g)
    {
      int m, n;
      for (; m <= n; m++)
	*d++ = m;
      k = 1;
      if (e)
	break;
      switch (*o)
	{
	case 'N':
	  o++;
	  if (c)
	    if (h)
	      while (i)
		{
		  s (-l, ~0);
		  t (j);
		  d = d + (a & 10000000 ? u (r, 2) : b);
		}
	}
      if (*o)
	p ? s () : 0;
    }
}
