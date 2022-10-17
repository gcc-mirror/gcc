/* { dg-do compile { target fgraphite } } */
/* { dg-options "-O1 -floop-parallelize-all -ftree-parallelize-loops=2 -fno-tree-dominator-opts" } */

void
foo (int x, int y)
{
  int i;

  for (i = 0; i < 2; i++)
    {
    }

  while (x)
    if (!y)
      while (y)
        ++y;
}
