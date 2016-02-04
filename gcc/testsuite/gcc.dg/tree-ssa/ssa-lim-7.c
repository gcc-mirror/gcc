/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim2-details" } */

extern const int srcshift;

void foo (int *srcdata, int *dstdata)
{
  int i;

  for (i = 0; i < 256; i++)
    dstdata[i] = srcdata[i] << srcshift;
}

/* { dg-final { scan-tree-dump "Moving statement" "lim2" } } */
