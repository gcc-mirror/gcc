/* Formerly known as ltrans-5.c */

int foo ()
{
  int A[100][1111];
  int i, j;

  for( i = 0; i < 1111; i++)
    for( j = 0; j < 100; j++)
      A[j][i] = 5 * A[j][i];

  return A[10][10];
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
