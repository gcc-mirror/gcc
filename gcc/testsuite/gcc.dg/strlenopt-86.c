/* PR tree-optimization/83821 - local aggregate initialization defeats
   strlen optimization
   Verify that a strlen() call is not eliminated for a pointer to a region
   of memory allocated by calloc() if a byte is written into the region
   that isn't known to be nul.
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

unsigned n0, n1;

void*
keep_strlen_calloc_store_cst_memset (int i, unsigned a, unsigned b)
{
  char *p = __builtin_calloc (a, 1);

  p[i] = 'x';

  __builtin_memset (p, 0, b);

  n0 = __builtin_strlen (p);

  return p;
}

void*
keep_strlen_calloc_store_var_memset (int i, int x, unsigned a, unsigned b)
{
  char *p = __builtin_calloc (a, 1);

  p[i] = x;

  __builtin_memset (p, 0, b);

  n0 = __builtin_strlen (p);

  return p;
}

void*
keep_strlen_calloc_store_memset_2 (int i, int x, unsigned a, unsigned b, unsigned c)
{
  char *p = __builtin_calloc (a, 1);

  p[i] = x;
  __builtin_memset (p, 0, b);

  n0 = __builtin_strlen (p);

  p[3] = x;
  __builtin_memset (p, 0, c);

  n1 = __builtin_strlen (p);

  return p;
}

/* { dg-final { scan-tree-dump-times "__builtin_strlen" 4 "optimized" } } */
