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

/* { dg-final { scan-tree-dump "MEM\\\[.* \\+ 252B\\\]" "optimized" { target int32plus } } } */
/* { dg-final { scan-tree-dump "MEM\\\[.* \\+ 126B\\\]" "optimized" { target int16 } } } */
