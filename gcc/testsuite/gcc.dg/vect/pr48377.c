/* PR tree-optimization/48377 */
/* { dg-require-effective-target non_strict_align } */

typedef unsigned int U __attribute__((__aligned__ (1), __may_alias__));

__attribute__((noinline, noclone)) unsigned int
foo (const char *s, int len)
{
  const U *p = (const U *) s;
  unsigned int f = len / sizeof (unsigned int), hash = len, i;

  for (i = 0; i < f; ++i)
    hash += *p++;
  return hash;
}

char buf[64] __attribute__((aligned (32)));

int
main (void)
{
  return foo (buf + 1, 26) != 26;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
