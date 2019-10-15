/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-vectorize -fno-tree-dce -fno-tree-sink -w" } */

int a8;

void
c1 (int oz, int dk, int ub)
{
  int *hd = 0;
  long int *th = &dk;

  while (ub < 1)
    {
      oz || dk;
      ++ub;
    }

  while (oz < 2)
    {
      long int *lq = &oz;

      (*hd < (*lq = *th)) < oz;

      if (oz == 0)
        *th = a8 = oz;

      *lq = 0;
    }
}
