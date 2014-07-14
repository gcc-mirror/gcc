/* { dg-do run } */

#include <stdlib.h>
int a, b, c, e, f, g, h, i, j, k;
int d[1];

int
main ()
{
  int l, m;
  k = 0;
  for (; g < 1; g++)
    {
      for (i = 0; i < 1; i++)
	m = b ? b : 1;
      if (m)
	j = d[e] = 0;
      else
	f = 0;
      l = k ? k : a;
      if (d[0] < 1)
	{
	  c++;
	  h = (l || e) > 0;
	}
    }
  if (c != 1)
    abort();
  return 0;
}