/* { dg-do compile } */
/* { dg-require-effective-target offload_hsa } */
/* { dg-options "-fopenmp -fdump-tree-omplower-details" } */

void
foo1 (int n, int *a, int workgroup_size)
{
  int i;
#pragma omp target
#pragma omp teams thread_limit(workgroup_size)
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i)
    for (i = 0; i < n; i++)
      a[i]++;
}

void
foo2 (int j, int n, int *a)
{
  int i;
#pragma omp target teams
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i) firstprivate(j)
    for (i = j + 1; i < n; i++)
      a[i] = i;
}

void
foo3 (int j, int n, int *a)
{
  int i;
#pragma omp target teams
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i) firstprivate(j)
  for (i = j + 1; i < n; i += 3)
    a[i] = i;
}

void
foo4 (int j, int n, int *a)
{
#pragma omp parallel
  {
    #pragma omp single
    {
      int i;
#pragma omp target
#pragma omp teams
#pragma omp distribute parallel for shared(a) firstprivate(n) private(i) firstprivate(j)
      for (i = j + 1; i < n; i += 3)
	a[i] = i;
    }
  }
}


/* { dg-final { scan-tree-dump-times "Target construct will be turned into a gridified HSA kernel" 4 "omplower" } } */
