/* PR middle-end/58809 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

_Complex int j;
_Complex double d;

void
foo (void)
{
  #pragma omp parallel reduction (&:j)	/* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (|:j)	/* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (^:j)	/* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (min:j) /* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (max:j) /* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (&:d)	/* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (|:d)	/* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (^:d)	/* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (min:d) /* { dg-error "has invalid type for" } */
    ;
  #pragma omp parallel reduction (max:d) /* { dg-error "has invalid type for" } */
    ;
}
