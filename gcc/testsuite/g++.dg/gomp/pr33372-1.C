// PR c++/33372
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T>
void f ()
{
  extern T n ();
#pragma omp parallel num_threads(n)	// { dg-error "'num_threads' expression must be integral" }
  ;
#pragma omp parallel for schedule(static, n)
  for (int i = 0; i < 10; i++)		// { dg-error "chunk size expression must be integral" }
    ;
}

void g ()
{
  f<int> ();
}
