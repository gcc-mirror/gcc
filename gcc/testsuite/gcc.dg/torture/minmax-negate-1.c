/* { dg-do run } */
/* { dg-require-effective-target int32 } */

/* The canonicalization must not change any value.  */

static int __attribute__ ((noipa))
sub_min_cst (int a, int x)
{
  int nx = -x;
  return a - (nx < 5 ? nx : 5);
}

static int __attribute__ ((noipa))
sub_max_cst (int a, int x)
{
  int nx = -x;
  return a - (nx > 5 ? nx : 5);
}

static int __attribute__ ((noipa))
sub_min_two_uses (int a, int x, int y, int *p)
{
  int nx = -x;
  int ny = -y;
  *p = nx;
  return a - (nx < ny ? nx : ny);
}

int
main (void)
{
  static const int vs[] = { -1000000, -257, -6, -5, -4, -1, 0, 1, 4, 5, 6,
			    257, 1000000 };
  unsigned n = sizeof (vs) / sizeof (vs[0]);
  for (unsigned i = 0; i < n; i++)
    for (unsigned j = 0; j < n; j++)
      {
	int a = vs[i], x = vs[j], s;
	if (sub_min_cst (a, x) != a - (-x < 5 ? -x : 5))
	  __builtin_abort ();
	if (sub_max_cst (a, x) != a - (-x > 5 ? -x : 5))
	  __builtin_abort ();
	for (unsigned k = 0; k < n; k++)
	  {
	    int y = vs[k];
	    if (sub_min_two_uses (a, x, y, &s) != a - (-x < -y ? -x : -y)
		|| s != -x)
	      __builtin_abort ();
	  }
      }
  return 0;
}
