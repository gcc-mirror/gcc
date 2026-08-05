/* PR c++/96867 */

int *v;

void
foo (int x)
{
  #pragma omp target update to (x, v[:])	/* { dg-error "for pointer type length expression must be specified" } */
}
