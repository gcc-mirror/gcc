/* See also 'if-clause-2.c'.  */

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

/* The same, but with implicit 'true' condition-argument.  */

void
g (short d)
{
#pragma acc parallel self copy(d)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "gimple" } } */
  ++d;

#pragma acc kernels self copy(d)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_kernels map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "gimple" } } */
  ++d;

#pragma acc serial self copy(d)
  /* { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_serial map\(tofrom:d \[len: [0-9]+\]\) self\(1\)$} 1 "gimple" } } */
  ++d;
}
