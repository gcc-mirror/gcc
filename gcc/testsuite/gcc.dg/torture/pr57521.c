/* { dg-do run } */
/* { dg-options "-ftree-loop-if-convert" } */

void abort (void);

int a, b, c, d, o = 1, p;
short e;

int
fn1 (int * p1)
{
  int f, g, h, j = 0, k = 0, l = 0;
  unsigned int i;
  int *m[1] = { &l };
  for (; b >= 0; b--)
    {
      if (*p1)
	if (j >= 0)
	  {
	    int n = 1;
	    e = 1;
	    h = a ? a : 1 % n;
	    g = h > 0 ? 0 : h + 1;
	    k = c + g;
	  }
	else
	  continue;
      else
	{

	  f = d > 0 ? 0 : d + 1;
	  i = f;
	  j = 1 + i;
	}
      l++;
    }
  return k;
}

int
main ()
{
  for (;; p++)
    {
      fn1 (&o);
      break;
    }
  if (e != 1)
    abort ();
  return 0;
}
