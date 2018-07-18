/* { dg-do assemble } */
/* { dg-options "-O1 -w" } */

static long long a[9];
int b, c, d, e, g;

static int
fn1 (int *p1)
{
  b = 1;
  for (; b >= 0; b--)
    {
      d = 0;
      for (;; d++)
	{
	  e && (a[d] = 0);
	  if (*p1)
	    break;
	  c = (int) a;
	}
    }
  return 0;
}

int
main ()
{
  int f = fn1 ((int *) f);
  return f;
}
