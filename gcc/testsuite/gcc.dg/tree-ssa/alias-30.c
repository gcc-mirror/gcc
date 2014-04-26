/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

extern int posix_memalign(void **memptr,
			  __SIZE_TYPE__ alignment, __SIZE_TYPE__ size);

int foo (float *p)
{
  int res = *p;
  int *q;
  void *tem;
  if (posix_memalign (&tem, 128, 128 * sizeof (int)) != 0)
    return 0;
  q = (int *)tem;
  *q = 1;
  return res + *p;
}

/* We should be able to CSE the load from *p in the return stmt.  */

/* { dg-final { scan-tree-dump "Replaced \\\*p" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
