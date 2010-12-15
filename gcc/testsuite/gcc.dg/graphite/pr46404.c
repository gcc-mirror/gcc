/* { dg-options "-O -fgraphite-identity -fno-tree-scev-cprop" } */

int l[200];

void
foo (void)
{
  int i = 0;
  for (; i < 100; i++)
    ;
  for (; i; i--)
    l[i];
}
