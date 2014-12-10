/* { dg-options "-O -fgraphite-identity -fno-tree-scev-cprop" } */

int foo (void);

int
huft_build (unsigned *b)
{
  int k;
  for (k = 0; k <= 10; k++)
    if (foo ());
}
int
inflate_fixed ()
{
  int i;
  unsigned l[288];
  for (i = 0; i < 144; i++)
    l[i] = 8;
  for (; i < 256; i++)
    l[i] = 9;
  for (; i < 280; i++)
    l[i] = 7;
  for (; i < 288; i++)
    l[i] = 8;
  if ((i = huft_build (l)) != 0)
    return i;
  for (i = 0; i < 30; i++)
    l[i] = 5;
}
