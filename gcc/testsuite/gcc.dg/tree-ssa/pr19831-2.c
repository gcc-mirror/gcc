/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

void test1(void)
{
  int *p = __builtin_malloc (sizeof (int) * 4);
  *p++ = 4;
  __builtin_free (p);
}

/* Undefined.  We can't do anything here.  */

/* { dg-final { scan-tree-dump-times "free" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "malloc" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
