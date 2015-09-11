/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void f (const char *c, int *i)
{
  *i = 42;
  __builtin_memcpy (i - 1, c, sizeof (int));
  if (*i != 42) __builtin_abort();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */

