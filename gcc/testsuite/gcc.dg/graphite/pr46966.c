/* PR tree-optimization/46966 */
/* { dg-do compile } */
/* { dg-options "-O -floop-interchange -ffast-math -fno-tree-copy-prop -fno-tree-loop-im" } */

int a[1000][1000];

void foo ()
{
  int i, j;
  for (i = 0; i < 1000; i++)
    for (j = 0; j < 1000; j++)
      a[i][j] = 0;
}
