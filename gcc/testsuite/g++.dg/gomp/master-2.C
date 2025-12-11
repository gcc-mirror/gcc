/* { dg-do compile } */
// { dg-additional-options "-Wno-deprecated-openmp" }
void f1(void)
{
  #pragma omp master asdf	/* { dg-error "expected" } */
  #pragma omp master
}				/* { dg-error "expected" } */
