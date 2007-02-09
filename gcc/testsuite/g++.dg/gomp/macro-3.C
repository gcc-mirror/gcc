// PR preprocessor/27746
// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-omplower" }

#define omp		FOO
#define p		parallel
#define omp_parallel	_Pragma ("omp parallel")
#define omp_p		_Pragma ("omp p")

void bar (void);

void
foo (void)
{
  #pragma omp parallel
    bar ();
  #pragma omp p
    bar ();
  omp_parallel
    bar ();
  omp_p
    bar ();
}

// { dg-final { scan-tree-dump-times "#pragma omp parallel" 4 "omplower" } }
// { dg-final { cleanup-tree-dump "omplower" } }
