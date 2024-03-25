/* See also 'if-clause-2.c'.  */

/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-additional-options "--param=openacc-kernels=decompose" }
   { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose" } */

void
f (short c)
{
#pragma acc parallel self(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "gimple" } } */
  ++c;

#pragma acc kernels self(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_kernels map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_data_kernels map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "omp_oacc_kernels_decompose" } }
     { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel_kernels_gang_single async\(-1\) num_gangs\(1\) map\(force_present:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "omp_oacc_kernels_decompose" } } */
  ++c;

#pragma acc serial self(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_serial map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "gimple" } } */
  ++c;
}

/* The same, but with implicit 'true' condition-argument.  */

void
g (short d)
{
#pragma acc parallel self copy(d)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "gimple" } } */
  ++d;

#pragma acc kernels self copy(d)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_kernels map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_data_kernels map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "omp_oacc_kernels_decompose" } }
     { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel_kernels_gang_single async\(-1\) num_gangs\(1\) map\(force_present:d \[len: [0-9]+\]\) self\(1+\)$} 1 "omp_oacc_kernels_decompose" } } */
  ++d;

#pragma acc serial self copy(d)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_serial map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "gimple" } } */
  ++d;
}
