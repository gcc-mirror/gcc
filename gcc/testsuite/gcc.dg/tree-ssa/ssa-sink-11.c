/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define SIZE 64

int foo (int v[], int a)
{
  int r, i;

  for (i = 0; i < SIZE; i++)
    r = v[i] + a;

  return r;
}

/* { dg-final { scan-tree-dump "MEM\\\[.* \\+ 252B\\\]" "optimized"} } */
