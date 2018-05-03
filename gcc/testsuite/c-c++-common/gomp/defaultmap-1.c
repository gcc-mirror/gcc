void
foo (void)
{
  #pragma omp target defaultmap(alloc) defaultmap(alloc)	/* { dg-error "too many 'defaultmap' clauses with unspecified category" } */
  ;
  #pragma omp target defaultmap(to) defaultmap(from)		/* { dg-error "too many 'defaultmap' clauses with unspecified category" } */
  ;
  #pragma omp target defaultmap(tofrom) defaultmap(firstprivate:scalar)	/* { dg-error "too many 'defaultmap' clauses with 'scalar' category" } */
  ;
  #pragma omp target defaultmap(none:aggregate) defaultmap(alloc:scalar) defaultmap(none:scalar) /* { dg-error "too many 'defaultmap' clauses with 'scalar' category" } */
  ;
  #pragma omp target defaultmap(none : pointer) defaultmap ( none ) /* { dg-error "too many 'defaultmap' clauses with 'pointer' category" } */
  ;
  #pragma omp target defaultmap()				/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(for)				/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(blah)				/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(tofrom:)			/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(tofrom scalar)			/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(tofrom,scalar)			/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(default ;)			/* { dg-error "expected" } */
  ;
  #pragma omp target defaultmap(default : qux)			/* { dg-error "expected" } */
  ;
}
