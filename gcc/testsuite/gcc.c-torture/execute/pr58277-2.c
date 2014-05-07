/* PR tree-optimization/58277 */

extern void abort (void);
static int a[1], b, c, e, i, j, k, m, q[] = { 1, 1 }, t;
int volatile d;
int **r;
static int ***volatile s = &r;
int f, g, o, x;
static int *volatile h = &f, *p;
char n;

static void
fn1 ()
{
  b = a[a[a[a[a[a[a[a[b]]]]]]]];
  b = a[a[a[a[a[a[a[a[b]]]]]]]];
  b = a[a[b]];
  b = a[a[a[a[a[a[a[a[b]]]]]]]];
  b = a[a[a[a[a[a[a[a[b]]]]]]]];
}

static int
fn2 ()
{
  n = 0;
  for (; g; t++)
    {
      for (;; m++)
	{
	  d;
	  int *u;
	  int **v[] = {
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	    0, 0, 0, 0, 0, &u, 0, 0, 0, 0, &u, &u, &u, &u, &u, &u, &u, 0,
	    &u, 0, &u, &u, &u, 0, &u, &u, 0, &u, &u, &u, &u, 0, &u, &u, &u,
	    &u, &u, 0, &u, &u, 0, &u, 0, &u, &u, 0, &u, &u, &u, &u, &u, 0,
	    &u, 0, 0, 0, &u, &u, &u, 0, 0, &u, &u, &u, 0, &u, 0, &u, &u
	  };
	  int ***w[] = { &v[0] };
	  if (*p)
	    break;
	  return 0;
	}
      *h = 0;
    }
  return 1;
}

static void
fn3 ()
{
  int *y[] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  for (; i; i++)
    x = 0;
  if (fn2 ())
    {
      int *z[6] = { };
      for (; n < 1; n++)
	*h = 0;
      int t1[7];
      for (; c; c++)
	o = t1[0];
      for (; e; e--)
	{
	  int **t2 = &y[0];
	  int ***t3 = &t2;
	  *t3 = &z[0];
	}
    }
  *s = 0;
  for (n = 0;; n = 0)
    {
      int t4 = 0;
      if (q[n])
	break;
      *r = &t4;
    }
}

int
main ()
{
  for (; j; j--)
    a[0] = 0;
  fn3 ();
  for (; k; k++)
    fn1 ();
  fn1 ();
 
  if (n)
    abort ();

  return 0;
}
