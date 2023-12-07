/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-loop-im --param=sccvn-max-alias-queries-per-access=0 -w" } */

int a8;

void
c1 (int oz, int dk, int ub)
{
  int *hd = 0;
  long int *th = (long int *) &dk;

  while (ub < 1)
    {
      oz || dk;
      ++ub;
    }

  while (oz < 2)
    {
      long int *lq = (long int *) &oz;

      (*hd < (*lq = *th)) < oz;

      if (oz == 0)
        *th = a8 = oz;

      *lq = 0;
    }
}
