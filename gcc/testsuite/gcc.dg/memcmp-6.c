/* PR tree-optimization/96670 - ICE on memchr with an empty initializer
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

struct {
  int i, j;
} const s = { };

const char a[sizeof s] = { };

void memcmp_success_unused (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  __builtin_memcmp (p, a, n);
  __builtin_memcmp (a, p, n);
}

void memcmp_success_used (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  if (__builtin_memcmp (p, a, n)
      || __builtin_memcmp (a, p, n))
    __builtin_abort ();
}

void memcmp_fail_unused (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  __builtin_memcmp (p, a, n);
  __builtin_memcmp (a, p, n);
}

void memcmp_fail_used (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  if (__builtin_memcmp (p, a, n)
      || __builtin_memcmp (a, p, n))
    __builtin_abort ();
}

/* { dg-prune-output "\\\[-Wunused-value" }
   { dg-final { scan-tree-dump-not "abort" "optimized" } }
   { dg-final { scan-tree-dump-not "memcmp \\\(" "optimized" } } */
