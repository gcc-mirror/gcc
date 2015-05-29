/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

void f1(void)
{
  #pragma omp flush
}

int x, y, z;

void f2(_Bool p)
{
  if (p)
    {
      #pragma omp flush (x)
    }
  else
    {
      #pragma omp flush (x, y, z)
    }
}

/* { dg-final { scan-tree-dump-times "__sync_synchronize" 3 "gimple" } } */
