/* The accesses to long and long long deliberately alias.  Check that
   predictive commoning does not use an initializer with a different alias
   set from the chain root.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

extern void *calloc (__SIZE_TYPE__, __SIZE_TYPE__);

int n, sink;

__attribute__((noinline)) void
g (char *p)
{
  *(long *) p = 3;
}

__attribute__((noinline)) void
f (char *p)
{
  *(long long *) p = 7;
}

int
main (void)
{
  char *b = calloc (80, 1);
  g (b);
  long prev = *(long *) b;
  f (b);
  long s = 0, t = 0;
  for (int k = 0; k < (n & 7) + 1; k++)
    {
      long long cur = *(long long *) (b + 8 + k * 8);
      s = s * 31 + *(long long *) (b + k * 8);
      t += prev;
      prev = cur;
      sink = k;
    }
  if (s != 7 || t != 3)
    __builtin_abort ();
  return 0;
}
