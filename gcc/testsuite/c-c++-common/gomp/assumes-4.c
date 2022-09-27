void
foo (void)
{
  #pragma omp assumes no_openmp		/* { dg-error "'#pragma omp assumes' may only be used at file scope" "" { target c } } */
  ;					/* { dg-error "'#pragma omp assumes' may only be used at file or namespace scope" "" { target c++ } .-1 } */
}
