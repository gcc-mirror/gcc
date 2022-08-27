/* { dg-do compile { target fgraphite } } */
/* { dg-options "-O -floop-parallelize-all -ftree-parallelize-loops=2 -fno-tree-dce" } */

void
foo (int x)
{
  int a[2];
  int b, c = 0;

  for (b = 0; b < 2; ++b)
    a[b] = 0;
  for (b = 0; b < 2; ++b)
    a[b] = 0;

  while (c < 1)
    while (x < 1)
      ++x;
}
