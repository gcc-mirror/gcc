/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -fno-tree-scev-cprop" } */

extern void bar(int);

void
foo (int i, int n)
{
  int a[30];
  int b[30];
  for (; i < n; i++)
    a[i] = b[i] = 0;

  while (1)
    if (b[0])
      bar (a[i - 1]);
}
