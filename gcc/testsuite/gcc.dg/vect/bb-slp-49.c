/* This checks that vectorized constructors have the correct ordering. */
/* { dg-require-effective-target vect_int } */

typedef int V __attribute__((__vector_size__(16)));

__attribute__((__noipa__)) void
foo (unsigned int x, V *y)
{
  unsigned int a[4] = { x + 0, x + 2, x + 4, x + 6 };
  for (unsigned int i = 0; i < 3; ++i)
    if (a[i] == 1234)
      a[i]--;
  *y = (V) { a[3], a[2], a[1], a[0] };
}

int
main ()
{
  V b;
  foo (0, &b);
  if (b[0] != 6 || b[1] != 4 || b[2] != 2 || b[3] != 0)
    __builtin_abort ();
  return 0;
}

/* See that we try to vectorize an SLP instance.  */
/* { dg-final { scan-tree-dump "Analyzing vectorizable constructor" "slp1" } } */
