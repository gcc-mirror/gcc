/* PR tree-optimization/79715 - hand-rolled strdup with unused result
   not eliminated
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

void f (const char *s)
{
  unsigned n = __builtin_strlen (s) + 1;
  char *p = __builtin_malloc (n);
  __builtin_memcpy (p, s, n);
  __builtin_free (p);
}

void g (const char *s)
{
  unsigned n = __builtin_strlen (s) + 1;
  char *p = __builtin_malloc (n);
  __builtin_strcpy (p, s);
  __builtin_free (p);
}

/* { dg-final { scan-tree-dump-not "free" "optimized" } }
   { dg-final { scan-tree-dump-not "malloc" "optimized" } }
   { dg-final { scan-tree-dump-not "memcpy" "optimized" } }
   { dg-final { scan-tree-dump-not "strcpy" "optimized" } }
   { dg-final { scan-tree-dump-not "strlen" "optimized" } } */
