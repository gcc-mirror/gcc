/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int x = 6;

int
main ()
{
  int v;
  #pragma omp atomic
    x = x * 7 + 6;	/* { dg-error "expected|invalid form of" } */
  #pragma omp atomic
    x = x * 7 ^ 6;	/* { dg-error "expected|invalid form of" } */
  #pragma omp atomic update
    x = x - 8 + 6;	/* { dg-error "expected|invalid form of" } */
  #pragma omp atomic
    x = x ^ 7 | 2;	/* { dg-error "expected|invalid form of" } */
  #pragma omp atomic
    x = x / 7 * 2;	/* { dg-error "expected|invalid form of" } */
  #pragma omp atomic
    x = x / 7 / 2;	/* { dg-error "expected|invalid form of" } */
  #pragma omp atomic capture
    { v = x; x = x * 7 + 6; }	/* { dg-error "expected" "" { target c++ } } */
  #pragma omp atomic capture
    { v = x; x = x * 7 ^ 6; }	/* { dg-error "expected" "" { target c++ } } */
  #pragma omp atomic capture
    { v = x; x = x - 8 + 6; }	/* { dg-error "expected" "" { target c++ } } */
  #pragma omp atomic capture
    { v = x; x = x ^ 7 | 2; }	/* { dg-error "expected" "" { target c++ } } */
  #pragma omp atomic capture
    { v = x; x = x / 7 * 2; }	/* { dg-error "expected" "" { target c++ } } */
  #pragma omp atomic capture
    { v = x; x = x / 7 / 2; }	/* { dg-error "expected" "" { target c++ } } */
  #pragma omp atomic capture
    { x = x * 7 + 6; v = x; }	/* { dg-error "expected|uses two different expressions for memory" } */
  #pragma omp atomic capture
    { x = x * 7 ^ 6; v = x; }	/* { dg-error "expected|uses two different expressions for memory" } */
  #pragma omp atomic capture
    { x = x - 8 + 6; v = x; }	/* { dg-error "expected|uses two different expressions for memory" } */
  #pragma omp atomic capture
    { x = x ^ 7 | 2; v = x; }	/* { dg-error "expected|uses two different expressions for memory" } */
  (void) v;
  return 0;
}
