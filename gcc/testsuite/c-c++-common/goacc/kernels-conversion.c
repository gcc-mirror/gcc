/* { dg-additional-options "-fopenacc-kernels=split" } */
/* { dg-additional-options "-fdump-tree-convert_oacc_kernels" } */

#define N 1024

unsigned int a[N];

int
main (void)
{
  int i;
  unsigned int sum = 1;

#pragma acc kernels copyin(a[0:N]) copy(sum)
  {
    #pragma acc loop
    for (i = 0; i < N; ++i)
      sum += a[i];

    sum++;
    a[0]++;

    #pragma acc loop
    for (i = 0; i < N; ++i)
      sum += a[i];
  }

  return 0;
}

/* Check that the kernels region is split into a data region and enclosed
   parallel regions.  */ 
/* { dg-final { scan-tree-dump-times "oacc_data_kernels" 1 "convert_oacc_kernels" } } */

/* The two loop regions are parallelized, the sequential part in between is
   made gang-single.  */
/* { dg-final { scan-tree-dump-times "oacc_parallel_kernels_parallelized" 2 "convert_oacc_kernels" } } */
/* { dg-final { scan-tree-dump-times "oacc_parallel_kernels_gang_single" 1 "convert_oacc_kernels" } } */

/* Check that the original kernels region is removed.  */
/* { dg-final { scan-tree-dump-not "oacc_kernels" "convert_oacc_kernels" } } */
