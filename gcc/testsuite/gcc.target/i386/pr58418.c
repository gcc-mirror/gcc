/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
int a, b, *c = &b, d = -1, e, f, *g, *h = &f, **i = &g, j;

unsigned int
foo (unsigned int p)
{
  return p == 0 ? 0 : 1 / p;
}

static int *
bar ()
{
  *c = *h = foo (d) & (-9 < d);
  for (e = 0; e; e++)
    ;
  return 0;
}

int
main ()
{
  for (; j; j++)
    for (;; a--)
      ;
  *i = bar ();
  if (f != 0)
    abort ();
  return 0;
}
