// PR c++/33372
// { dg-do compile }
// { dg-options "-fopenmp" }

template <typename T>
void f ()
{
  T n = 6;
#pragma omp parallel num_threads(n)	// { dg-error "'num_threads' expression must be integral" }
  ;
#pragma omp parallel for schedule(static, n)	// { dg-error "chunk size expression must be integral" }
  for (int i = 0; i < 10; i++)
    ;
}

void g ()
{
  f<double> ();
}
