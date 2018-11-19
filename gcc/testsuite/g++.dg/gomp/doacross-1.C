// { dg-do compile { target c++11 } }
// { dg-options "-fopenmp" }

int a[42];

void
foo ()
{
  #pragma omp for ordered (1)	// { dg-error "'ordered' clause with parameter on range-based 'for' loop" }
  for (auto x : a)
    ;
}

void
bar ()
{
  #pragma omp for ordered (2)	// { dg-error "'ordered' clause with parameter on range-based 'for' loop" }
  for (int i = 0; i < 1; i++)
    for (auto x : a)
      ;
}
