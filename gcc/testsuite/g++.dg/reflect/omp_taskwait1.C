// { dg-do compile { target c++26 } }
// { dg-require-effective-target fopenmp }
// { dg-additional-options "-freflection -fopenmp" }

void
foo (int *x)
{
  ++x;
  [[omp::directive (taskwait depend (inout: x[0]))]];
}
