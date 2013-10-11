// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-omplower" }

void bar();
void foo()
{
  #pragma omp critical
    bar ();
  #pragma omp master
    bar ();
  #pragma omp single
    bar ();
  #pragma omp for
  for (int i = 0; i < 10; ++i)
    bar ();
  #pragma omp sections
    { bar(); }
  #pragma omp parallel
    bar ();
  #pragma omp parallel for
  for (int i = 0; i < 10; ++i)
    bar ();
  #pragma omp parallel sections
    {
      {
	bar ();
	bar ();
      }
    #pragma omp section
      bar ();
    }
}

// { dg-final { scan-tree-dump-times "terminate" 10 "omplower" } }
// { dg-final { cleanup-tree-dump "omplower" } }
