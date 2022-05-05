/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

void
foo ()
{
  int *x, *y;

  #pragma omp target data map(x, y) use_device_ptr(x, y)
    #pragma omp target is_device_ptr(x, y)
      {
	*x = 42;
      }
}

/* { dg-final { scan-tree-dump "is_device_ptr\\(x\\)"  "gimple" } } */
/* { dg-final { scan-tree-dump-not "is_device_ptr\\(y\\)"  "gimple" } } */
