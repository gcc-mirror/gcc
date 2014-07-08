/* { dg-do run } */

extern void abort (void);

int a = 1, *e = &a, **f = &e, *l, *p, j;
static int b;
long d;
short g;

void
fn1 (int *p)
{
  int m;
  if (!(*p & j))
    {
      int *n = &m;
      for (d = 6; d; d--)
	{
	  for (g = 0; g < 1; g++)
	    {
	      n = l = *f;
	      b = *p;
	    }
	  *n = 0;
	}
    }
}

int
main ()
{
  p = *f;
  fn1 (p);
  if (b != 0)
    abort ();
  return 0;
}
