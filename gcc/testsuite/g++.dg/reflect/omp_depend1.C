// { dg-do compile { target c++26 } }
// { dg-require-effective-target fopenmp }
// { dg-additional-options "-freflection -fopenmp" }

int *foo (int, int);

void
bar (int *x)
{
  [[omp::directive (parallel)]]
  for (int i = 0; i < 64; i++)
    [[omp::directive (task, depend (iterator (j=i:i+1), out: foo (1, j)[0]))]]
    x[i] = i;
}
