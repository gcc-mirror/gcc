// PR middle-end/34608
// { dg-do compile }
// { dg-options "-ftest-coverage -fopenmp" }

struct A
{
  ~A () throw ();
};

void foo (A);

void
bar ()
{
#pragma omp parallel
  foo (A ());
}

// { dg-final { cleanup-coverage-files } }
