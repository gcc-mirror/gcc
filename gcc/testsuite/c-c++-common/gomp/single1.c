/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (int i)
{
  #pragma omp single copyprivate (i)
    ;
  #pragma omp single nowait
    ;
  #pragma omp single copyprivate (i) nowait	/* { dg-error "clause must not be used together with" } */
    ;
  #pragma omp single nowait copyprivate (i)	/* { dg-error "clause must not be used together with" } */
    ;
}
