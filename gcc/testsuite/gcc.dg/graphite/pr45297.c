/* { dg-options "-Os -fgraphite-identity" } */

void
foo (int *p)
{
  int *q = p + 1024;
  while (q != p)
    *--q = *--q;
}
