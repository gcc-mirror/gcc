/* { dg-additional-options "-fopenacc-kernels=split" } */
/* { dg-additional-options "-fdump-tree-convert_oacc_kernels" } */

void
f (short c)
{
#pragma acc parallel if(c)
  ;
#pragma acc kernels if(c)
  ;
#pragma acc data if(c)
  ;
#pragma acc update device(c) if(c)
}

/* Verify that the 'if' clause gets duplicated.
   { dg-final { scan-tree-dump-times "#pragma omp target oacc_data_kernels if\\(" 1 "convert_oacc_kernels" } }
   { dg-final { scan-tree-dump-times "#pragma omp target oacc_parallel_kernels_gang_single .* if\\(" 1 "convert_oacc_kernels" } } */
