/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

void
test (int *a, int *b, int * __restrict__ v)
{
  *a = *v;
  *b = *v;
}

/* { dg-final { scan-tree-dump-times "= \\*v" 1 "fre1" } } */
