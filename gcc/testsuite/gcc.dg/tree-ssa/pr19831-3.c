/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */

void test2(void)
{
  int *p = __builtin_malloc (sizeof (int) * 4);
  if (p == (void *)0)
    __builtin_abort ();
  __builtin_free (p);
}

void test5(int b)
{
  int *p = __builtin_malloc (sizeof (int) * 4);
  if (p)
    __builtin_free (p);
}

void test6(void)
{
  int *p = __builtin_malloc (sizeof (int) * 4);
  if (p == (void *)0)
    __builtin_abort ();
  if (p)
    __builtin_free (p);
}

/* We should be able to remove all malloc/free pairs with CDDCE.
   Assume p was non-NULL for test2.
   For test5, it doesn't matter if p is NULL or non-NULL.  */

/* { dg-final { scan-tree-dump-times "free" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "malloc" 0 "optimized" } } */
