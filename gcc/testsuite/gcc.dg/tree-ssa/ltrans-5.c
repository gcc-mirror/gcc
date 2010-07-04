/* { dg-do compile { target { size32plus } } } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all -march=i486" { target { i?86-*-* && ilp32} } } */

int foo ()
{
  int A[100][1111];
  int i, j;

  for( i = 0; i < 1111; i++)
    for( j = 0; j < 100; j++)
      A[j][i] = 5 * A[j][i];

  return A[10][10];
}

/* { dg-final { scan-tree-dump-times "transformed loop" 1 "ltrans"} } */ 
/* { dg-final { cleanup-tree-dump "ltrans" } } */
