/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int *
fx (int *b, int *e)
{
  return b + (e - b);
}

/* { dg-final { scan-tree-dump-not "/\\\[ex\\\]" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
