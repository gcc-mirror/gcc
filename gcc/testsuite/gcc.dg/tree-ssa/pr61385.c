/* { dg-do compile } */
/* { dg-options "-O2" } */

#define assert(x) if (!(x)) __builtin_abort ()

int a, b, c, d, e, f, g;

int
fn1 ()
{
  int *h = &c;
  for (; c < 1; c++)
    {
      int *i = &a, *k = &a;
      f = 0;
      if (b)
	return 0;
      if (*h)
	{
	  int **j = &i;
	  *j = 0;
	  d = 0;
	}
      else
	g = e = 0;
      if (*h)
	{
	  int **l = &k;
	  *l = &g;
	}
      d &= *h;
      assert (k == &a || k);
      assert (i);
    }
  return 0;
}

int
main ()
{
  fn1 (); 
  return 0;
}
