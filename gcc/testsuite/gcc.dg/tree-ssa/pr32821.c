/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-ifcvt-details" } */

void
main1 (int *arr, int n, int a, int b)
{
  int i;
  for (i = 0; i < n; i++)
    {
      int m = arr[i];
      arr[i] = (m < a ? m - a : b);
    }
}

