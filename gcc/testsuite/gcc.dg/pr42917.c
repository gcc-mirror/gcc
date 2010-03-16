/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-linear -fcompare-debug -fdump-tree-ltrans" } */

extern int A[];

void
foo ()
{
  int i, j;
  for (i = 0; i < 4; i++)
    for (j = 255; j >= 0; j--)
      A[j] = 0;
}

/* { dg-final { scan-tree-dump "Successfully transformed loop" "ltrans" } } */
/* { dg-final { cleanup-tree-dump "ltrans" } } */
