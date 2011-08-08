/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int x = 6;

int
main ()
{
  int v;
  #pragma omp atomic
    x = x * 7 + 6;	/* { dg-error "expected" } */
  #pragma omp atomic
    x = x * 7 ^ 6;	/* { dg-error "expected" } */
  #pragma omp atomic update
    x = x - 8 + 6;	/* { dg-error "expected" } */
  #pragma omp atomic
    x = x ^ 7 | 2;	/* { dg-error "expected" } */
  #pragma omp atomic
    x = x / 7 * 2;	/* { dg-error "expected" } */
  #pragma omp atomic
    x = x / 7 / 2;	/* { dg-error "expected" } */
  #pragma omp atomic capture
    v = x = x | 6;	/* { dg-error "invalid operator" } */
  #pragma omp atomic capture
    { v = x; x = x * 7 + 6; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { v = x; x = x * 7 ^ 6; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { v = x; x = x - 8 + 6; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { v = x; x = x ^ 7 | 2; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { v = x; x = x / 7 * 2; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { v = x; x = x / 7 / 2; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { x = x * 7 + 6; v = x; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { x = x * 7 ^ 6; v = x; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { x = x - 8 + 6; v = x; }	/* { dg-error "expected" } */
  #pragma omp atomic capture
    { x = x ^ 7 | 2; v = x; }	/* { dg-error "expected" } */
  (void) v;
  return 0;
}
