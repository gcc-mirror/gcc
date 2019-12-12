/* { dg-do compile } */
/* { dg-options "-fopenmp -O -fexceptions -fnon-call-exceptions -fno-tree-fre" } */

int
s0 (void)
{
  int g6, oh = 0;
  int *a6 = &g6;

  (void) a6;

#pragma omp parallel for
  for (g6 = 0; g6 < 1; ++g6)
    {
      int zk;

      for (zk = 0; zk < 1; ++zk)
        {
          oh += zk / (zk + 1);

          for (;;)
            {
            }
        }

      a6 = &zk;
    }

  return oh;
}
