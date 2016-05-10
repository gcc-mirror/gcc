/* PR tree-optimization/70916 */

int a, b, c, d, i, k;
int static *e = &b, *j;
int **f;
int static ***g = &f;
int *h;
void
fn1 ()
{
  for (;;)
    {
      int l[1] = { };
      int m = (__UINTPTR_TYPE__) l;
      for (; d; d--)
	{
	  int ****n;
	  int *****o = &n;
	  i = a & 7 ? : a;
	  *e = (((*o = &g) != (int ****) g) < h[c], 0) || k;
	  if (*e)
	    {
	      **n = &j;
	      *e = (__UINTPTR_TYPE__) h;
	    }
	}
    }
}
