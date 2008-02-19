// PR c++/35078
// { dg-do compile }
// { dg-options "-fopenmp" }

template<int> void
foo ()
{
#pragma omp parallel for
  for (int& i = 0; i < 10; ++i)	// { dg-error "invalid type for iteration variable" }
    ;
}

void
bar ()
{
  int j = 0;
#pragma omp parallel for
  for (int& i = j; i < 10; ++i)	// { dg-error "invalid type for iteration variable" }
    ;
}
