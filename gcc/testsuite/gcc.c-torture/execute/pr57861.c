/* PR rtl-optimization/57861 */

extern void abort (void);
short a = 1, f;
int b, c, d, *g = &b, h, i, j;
unsigned int e;

static int
foo (char p)
{
  int k;
  for (c = 0; c < 2; c++)
    {
      i = (j = 0) || p;
      k = i * p;
      if (e < k)
	{
	  short *l = &f;
	  a = d && h;
	  *l = 0;
	}
    }
  return 0;
}

int
main ()
{
  *g = foo (a);
  if (a != 0)
    abort ();
  return 0;
}
