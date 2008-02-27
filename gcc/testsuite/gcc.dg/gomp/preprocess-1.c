/* { dg-do preprocess } */

void foo (void)
{
  int i1, j1, k1;
#define p parallel
#define P(x) private (x##1)
#define S(x) shared (x##1)
#define F(x) firstprivate (x##1)
#pragma omp p P(i) \
  S(j) \
  F(k)
  ;
}

/* { dg-final { scan-file preprocess-1.i "(^|\n)#pragma omp parallel private \\(i1\\) shared \\(j1\\) firstprivate \\(k1\\)($|\n)" } } */
