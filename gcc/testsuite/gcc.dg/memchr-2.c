/* PR tree-optimization/96670 - ICE on memchr with an empty initializer
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

struct {
  int i, j;
} const s = { };

void memchr_success_unused (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  __builtin_memchr (p, '\0', n);
}

void memchr_success_used (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  if (&s != __builtin_memchr (p, '\0', n))
    __builtin_abort ();
}

void memchr_fail_unused (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  __builtin_memchr (p, '\5', n);
}

void memchr_fail_used (void)
{
  int n = (char *)&s.j - (char *)&s;
  char *p = (char *)&s;
  if (__builtin_memchr (p, '\5', n))
    __builtin_abort ();
}

/* { dg-prune-output "\\\[-Wunused-value" }
   { dg-final { scan-tree-dump-not "abort" "optimized" } }
   { dg-final { scan-tree-dump-not "memcmp \\(" "optimized" } } */
