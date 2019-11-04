/* PR tree-optimization/83821 - local aggregate initialization defeats
   strlen optimization
   Verify that a memset() call to zero out a subregion of memory
   allocated by calloc() is eliminated even if a zero byte is written
   into it in between the two calls.  See the calloc-2.c test that
   verifies that the memset() calls isn't eliminated if the written
   value is non-zero.
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

void* elim_calloc_store_memset_1 (unsigned a, unsigned b)
{
  char *p = __builtin_calloc (a, 1);

  p[1] = '\0';

  __builtin_memset (p, 0, b);   // should be eliminated

  return p;
}

void* elim_calloc_store_memset_2 (unsigned a, unsigned b, unsigned c)
{
  char *p = __builtin_calloc (a, 1);

  p[1] = '\0';
  __builtin_memset (p, 0, b);   // should be eliminated

  p[3] = '\0';
  __builtin_memset (p, 0, c);   // should also be eliminated

  return p;
}

/* { dg-final { scan-tree-dump-not "malloc" "optimized" } }
   { dg-final { scan-tree-dump-times "_calloc \\\(" 2 "optimized" } }
   { dg-final { scan-tree-dump-not "_memset \\\(" "optimized" } } */
