/* { dg-do assemble } */
/* { dg-options "-Os -fdump-tree-ivopts -save-temps -marm" } */

void
tr5 (short array[], int n)
{
  int x;
  if (n > 0)
    for (x = 0; x < n; x++)
      array[x] = 0;
}

/* { dg-final { scan-tree-dump-times "PHI <" 1 "ivopts"} } */
/* { dg-final { object-size text <= 32 } } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
