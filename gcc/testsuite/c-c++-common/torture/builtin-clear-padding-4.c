/* { dg-require-effective-target alloca } */

/* PR libstdc++/88101 */

struct S { char a; short b; char c; };

__attribute__((noipa)) void
foo (int m, int n, int o)
{
  long double a1[m];
  long double a2[m];
  struct S b1[m][n];
  struct S b2[m][n];
  struct S c1[m][n][o];
  struct S c2[m][n][o];
  int i, j, k;
  __builtin_memset (&a1, 0, sizeof (a1));
  __builtin_memset (&a2, ~0, sizeof (a2));
  __builtin_memset (&b1, 0, sizeof (b1));
  __builtin_memset (&b2, ~0, sizeof (b2));
  __builtin_memset (&c1, 0, sizeof (c1));
  __builtin_memset (&c2, ~0, sizeof (c2));
  for (i = 0; i < m; i++)
    {
      a1[i] = 13.132L;
      a2[i] = 13.132L;
      for (j = 0; j < n; j++)
	{
	  b1[i][j].a = -1;
	  b1[i][j].b = -1;
	  b1[i][j].c = -1;
	  b2[i][j].a = -1;
	  b2[i][j].b = -1;
	  b2[i][j].c = -1;
	  for (k = 0; k < o; k++)
	    {
	      c1[i][j][k].a = -1;
	      c1[i][j][k].b = -1;
	      c1[i][j][k].c = -1;
	      c2[i][j][k].a = -1;
	      c2[i][j][k].b = -1;
	      c2[i][j][k].c = -1;
	    }
	}
    }
  __builtin_clear_padding (&a2);
  __builtin_clear_padding (&b2);
  __builtin_clear_padding (&c2);
  if (__builtin_memcmp (&a1, &a2, sizeof (a1))
      || __builtin_memcmp (&b1, &b2, sizeof (b1))
      || __builtin_memcmp (&c1, &c2, sizeof (c1)))
    __builtin_abort ();
}

int
main ()
{
  foo (5, 3, 4);
  foo (17, 2, 1);
  return 0;
}
