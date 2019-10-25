/* PR tree-optimization/83821 - local aggregate initialization defeats
   strlen optimization
   Verify that with DSE disabled, a memset() call to zero out a subregion
   of memory allocated by calloc() is not eliminated after a non-zero byte
   is written into it using memset() in between the two calls.
   { dg-do compile }
   { dg-options "-O2 -fno-tree-dse -fdump-tree-optimized" } */

char* keep_memset_calls (void)
{
  char *p = __builtin_calloc (12, 1);

  __builtin_memset (p + 5, 1, 2);   /* dead store (not eliminated) */

  __builtin_memset (p, 0, 12);      /* must not be eliminated */

  return p;
}

/* { dg-final { scan-tree-dump-not "malloc" "optimized" } }
   { dg-final { scan-tree-dump-times "_calloc \\\(" 1 "optimized" } }
   { dg-final { scan-tree-dump-times "_memset \\\(" 2 "optimized" } } */
