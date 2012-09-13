/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int a[256];
int *b, *c;
void foo (int i, int j)
{
  b = &a[i+j];
  c = &a[i+j];
}

/* We should remove the redundant address computation.  */

/* { dg-final { scan-tree-dump-times " = &a" 1 "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
