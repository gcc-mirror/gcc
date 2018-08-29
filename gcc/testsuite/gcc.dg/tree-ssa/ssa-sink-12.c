/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define SIZE 64

int foo (int v1[], int v2[])
{
  int r, i, j;

  for (j = 0; j < SIZE; j++)
    for (i = 0; i < SIZE; i++)
      r = v1[j] + v2[i];

  return r;
}

/* { dg-final { scan-tree-dump "MEM\\\[.* \\+ 252B\\\]" "optimized" { target int32plus } } } */
/* { dg-final { scan-tree-dump "MEM\\\[.* \\+ 126B\\\]" "optimized" { target int16 } } } */
