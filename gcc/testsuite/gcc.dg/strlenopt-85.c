/* PR tree-optimization/83821 - local aggregate initialization defeats
   strlen optimization
   Verify that a strlen() call is eliminated for a pointer to a region
   of memory allocated by calloc() even if one or more nul bytes are
   written into it.
   { dg-do compile }
   { dg-options "-O2 -fno-tree-vectorize -fdump-tree-optimized" } */

unsigned n0, n1;

void* elim_strlen_calloc_store_memset_1 (unsigned a, unsigned b)
{
  char *p = __builtin_calloc (a, 1);

  p[0] = '\0';
  p[1] = '\0';
  p[2] = '\0';
  p[3] = '\0';

  __builtin_memset (p, 0, b);

  n0 = __builtin_strlen (p);

  return p;
}

void* elim_strlen_calloc_store_memset_2 (unsigned a, unsigned b, unsigned c)
{
  char *p = __builtin_calloc (a, 1);

  p[1] = '\0';
  __builtin_memset (p, 0, b);

  n0 = __builtin_strlen (p);

  p[3] = 0;
  __builtin_memset (p, 0, c);

  n1 = __builtin_strlen (p);

  return p;
}

/* { dg-final { scan-tree-dump-not "__builtin_strlen" "optimized" } } */
