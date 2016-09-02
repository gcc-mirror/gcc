/* PR c/65467 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c11" } */

#pragma omp declare simd
int
f1 (_Atomic int x, int y)	/* { dg-warning "ignoring '#pragma omp declare simd' on function with '_Atomic' qualified non-'uniform' argument" } */
{
  return x + y;
}

#pragma omp declare simd uniform(x)
int
f2 (_Atomic int x, int y)
{
  return x + y;
}
