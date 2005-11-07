/* { dg-do compile } */

static int *** foo (int);

void
bar ()
{
  int ***p = foo (2);
}

extern int *nd;
extern int ***tc;
extern int *ap;
extern int *as;
extern float ss;

static int ***
foo (int Fc)
{
  int i, j, s, p, n, t;

  n = 0;
  for (s = 0; s < 4; s++)
    n += nd[s];

  for (i = 0; i < n; i++)
    {
      p = ap[i];
      s = as[i];
      for (j = 0; j < Fc; j++)
	tc[p][s][j] = i * ss + j;
    }

  return (tc);
}

/* { dg-final { cleanup-tree-dump "vect" } } */
