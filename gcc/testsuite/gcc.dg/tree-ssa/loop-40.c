/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ch2-details" } */

int mymax2(int *it, int *end)
{
  int max = *it;
  while (++it != end)
    if (*it > max)
      max = *it;
  return max;
}

/* { dg-final { scan-tree-dump "Duplicating header" "ch2" } } */
