/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-fopenmp -std=c23" { target { c } } } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 100

void
bar (int a[], int run_parallel, int run_guided)
{
  [[omp::directive (metadirective
		    when (user={condition(run_parallel)}: parallel))]]
  {
    int i;
  [[omp::directive (metadirective
	when (construct={parallel}, user={condition(run_guided)}:
	      for schedule(guided))
	when (construct={parallel}: for schedule(static)))]]
      for (i = 0; i < N; i++)
	a[i] = i;
   }
 }

/* The outer metadirective should be resolved at parse time.  */
/* The inner metadirective should be resolved during Gimplificiation.  */

/* { dg-final { scan-tree-dump-times "#pragma omp metadirective" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp for" 4 "original" } } */
/* { dg-final { scan-tree-dump-times "when \\(construct = .parallel" 4 "original" } } */
/* { dg-final { scan-tree-dump-times "otherwise:" 2 "original" } } */

/* { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" } } */
