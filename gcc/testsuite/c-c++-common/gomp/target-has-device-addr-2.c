/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

void
foo ()
{
  int x, y;

  #pragma omp target data map(x, y) use_device_addr(x, y)
    #pragma omp target has_device_addr(x, y)
      {
	x = 42;
      }
}

/* { dg-final { scan-tree-dump "has_device_addr\\(x\\)"  "gimple" } } */
/* { dg-final { scan-tree-dump-not "has_device_addr\\(y\\)"  "gimple" } } */
