/* { dg-do compile } */

#include <stdint.h>

int a[10][14], b, c, d, e, f, g, h, i;
void bar (void);
int
foo (int x)
{
  unsigned j;
  int k = 0, l;
  int m;
  if (h)
    m = 12;
  else
    m = 13;
  if (a[x][m])
    l = (intptr_t) foo;
  a[x][i] = l;
  while (c)
    {
      if (b)
	{
	  if (f)
	    k = 1;
	  bar ();
	}
      for (; d;)
	j++;
    }
  while (c)
    {
      if (a[x][12])
	{
	  if (g)
	    k = 1;
	  j++;
	}
      c = e;
    }
  return k;
}
