/* See also 'self-clause-2.c'.  */

/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-additional-options "--param=openacc-kernels=decompose" }
   { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose" } */

void
f (short c)
{
#pragma acc parallel if(c) copy(c)
  ++c;

#pragma acc kernels if(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_kernels map\(tofrom:c \[len: [0-9]+\]\) if\(_[0-9]+\)$} 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_data_kernels map\(tofrom:c \[len: [0-9]+\]\) if\(_[0-9]+\)$} 1 "omp_oacc_kernels_decompose" } }
     { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel_kernels_gang_single async\(-1\) num_gangs\(1\) map\(force_present:c \[len: [0-9]+\]\) if\(_[0-9]+\)$} 1 "omp_oacc_kernels_decompose" } } */
  ++c;

#pragma acc serial if(c) copy(c)
  ++c;

#pragma acc data if(c) copy(c)
  ++c;

#pragma acc update if(c) device(c)
}
