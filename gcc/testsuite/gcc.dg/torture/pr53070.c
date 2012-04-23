/* { dg-do compile } */
/* { dg-options "-ffast-math -ftree-loop-if-convert -fno-tree-loop-im" } */
int
foo (int c)
{
  int t = 0, i = 0;
  for (; i < 100; i++)
    t += c ? c : 1;
  return t;
}
