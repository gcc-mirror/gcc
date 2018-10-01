/* PR tree-optimization/86844 */
/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

void
foo (int *p)
{
  *p = 0;
  int one = 1;
  __builtin_memcpy ((char *) p + 3, &one, sizeof (int));
  *((char *)p + 4) = *((char *)p + 36);
  *((char *)p + 1) = 2;
}

/* { dg-final { scan-tree-dump-not "Merging successful" "store-merging" } } */
