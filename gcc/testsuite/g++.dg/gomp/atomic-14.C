// PR middle-end/45423
// { dg-do compile }
// { dg-options "-fopenmp" }

bool *baz (), atomicvar;

int
foo (void)
{
  #pragma omp atomic
    (*baz ())--;	// { dg-error "invalid use of Boolean" }
  #pragma omp atomic
    --(*baz ());	// { dg-error "invalid use of Boolean" }
  #pragma omp atomic
    atomicvar--;	// { dg-error "invalid use of Boolean" }
  #pragma omp atomic
    --atomicvar;	// { dg-error "invalid use of Boolean" }
  return 0;
}
