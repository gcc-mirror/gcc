/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2 -g -w" } */
/* { dg-require-effective-target alloca } */

static int foo ();

int
bar (int n)
{
  struct S { char a[n]; } x;
  __builtin_memset (x.a, 0, n);
  return foo (n, x);
}

static inline int
foo (int n, struct T { char a[n]; } b)
{
  int r = 0, i;
  for (i = 0; i < n; i++)
    r += b.a[i];
  return r;
}
