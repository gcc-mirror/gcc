/* { dg-options "-fsave-optimization-record -ftree-slp-vectorize --param ggc-min-expand=1 --param ggc-min-heapsize=1024" } */

void
en (void)
{
}

void
n4 (int zb)
{
  while (zb < 1)
    ++zb;
}
