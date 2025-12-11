/* PR c/106836 */
// { dg-additional-options "-Wno-deprecated-openmp" }
void
foo (void)
{
#pragma omp target parallel depend (source)	/* { dg-error "'depend\\\(source\\\)' is only allowed in 'omp ordered'" } */
  ;
#pragma omp taskwait
}
