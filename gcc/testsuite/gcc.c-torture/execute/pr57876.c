/* PR rtl-optimization/57876 */

extern void abort (void);
int a, b = 1, c, *d = &c, f, *g, h, j;
static int e;

int
main ()
{
  int i;
  for (i = 0; i < 2; i++)
    {
      long long k = b;
      int l;
      for (f = 0; f < 8; f++)
	{
	  int *m = &e;
	  j = *d;
	  h = a * j - 1;
	  *m = (h == 0) < k;
	  g = &l;
	}
    }
  if (e != 1)
    abort ();
  return 0;
}
