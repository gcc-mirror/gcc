/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2 -g -w" } */

static int foo ();

int
bar (int n)
{
  return foo (n, 2.0);
}

static inline int
foo (int n, struct T { char a[n]; } b)
{
  int r = 0, i;
  for (i = 0; i < n; i++)
    r += b.a[i];
  return r;
}
