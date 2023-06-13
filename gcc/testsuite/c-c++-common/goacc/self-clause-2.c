/* { dg-additional-options "-fdump-tree-gimple" } */

void
f (short c)
{
#pragma acc parallel self(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "gimple" } } */
  ++c;

#pragma acc kernels self(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_kernels map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "gimple" } } */
  ++c;

#pragma acc serial self(c) copy(c)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_serial map\(tofrom:c \[len: [0-9]+\]\) self\(_[0-9]+\)$} 1 "gimple" } } */
  ++c;
}
