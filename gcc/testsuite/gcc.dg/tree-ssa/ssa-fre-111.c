/* PR122435 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

void foo (unsigned *p)
{
  int i = *p;
  *(int *)p = i;
}

/* { dg-final { scan-tree-dump "Deleted redundant store" "fre1" } } */
