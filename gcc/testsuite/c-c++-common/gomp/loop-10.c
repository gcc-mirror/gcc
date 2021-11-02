int a, b;
#pragma omp threadprivate (a, b)

void
foo (void)
{
  #pragma omp for				/* { dg-error "threadprivate iteration variable 'a'" } */
  for (a = 0; a < 32; a++)
    ;
  #pragma omp parallel for collapse(2)		/* { dg-error "threadprivate iteration variable 'a'" "" { target c } } */
  for (a = 0; a < 32; a++)			/* { dg-error "threadprivate iteration variable 'b'" "" { target c } .-1 } */
    for (b = 0; b < 32; b++)			/* { dg-error "threadprivate iteration variable 'a'" "" { target c++ } .-1 } */
      ;						/* { dg-error "threadprivate iteration variable 'b'" "" { target c++ } .-2 } */
  #pragma omp simd				/* { dg-error "threadprivate iteration variable 'a'" } */
  for (a = 0; a < 32; a++)
    ;
  #pragma omp taskloop				/* { dg-error "threadprivate iteration variable 'a'" } */
  for (a = 0; a < 32; a++)
    ;
  #pragma omp loop bind(thread)			/* { dg-error "threadprivate iteration variable 'a'" } */
  for (a = 0; a < 32; a++)
    ;
}

void
bar (void)
{
  #pragma omp distribute collapse(2)		/* { dg-error "threadprivate iteration variable 'a'" } */
  for (a = 0; a < 32; a++)			/* { dg-error "threadprivate iteration variable 'b'" "" { target *-*-* } .-1 } */
    for (b = 0; b < a; b++)
      ;
  #pragma omp distribute parallel for simd	/* { dg-error "threadprivate iteration variable 'a'" "" { target c } } */
  for (a = 0; a < 32; a++)			/* { dg-error "threadprivate iteration variable 'a'" "" { target c++ } } */
    ;
}
