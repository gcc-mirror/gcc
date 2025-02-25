/* PR c/119000 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -Wunused-but-set-variable" } */

int
foo (void)
{
  int a = 1, b, c = 1, v;	/* { dg-warning "variable 'b' set but not used" } */
  #pragma omp atomic write
  v = a;
  #pragma omp atomic read
  b = v;
  #pragma omp atomic update
  v += c;
  return v;
}
