/* PR tree-optimization/86844 */
/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

void
foo (int *p)
{
  *p = 0;
  *((char *)p + 3) = 1;
  *((char *)p + 1) = 2;
  *((char *)p + 2) = *((char *)p + 38);
}

/* { dg-final { scan-tree-dump-times "Merging successful" 1 "store-merging" } } */
/* { dg-final { scan-tree-dump-times "New sequence of 1 stores to replace old one of 3 stores" 1 "store-merging" } } */
