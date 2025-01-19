/* { dg-do compile } */
/* { dg-additional-options "-foffload=disable -fdump-tree-optimized" } */

#include <stdlib.h>

static void
init (int n, double *a)
{
  for (int i = 0; i < n; i++)
    a[i] = (double) i;
}

static void
check (int n, double *a, double s)
{
  for (int i = 0; i < n; i++)
    if (a[i] != (double) i * s)
      abort ();
}

typedef void (transform_fn) (int, double *, double);

static void doit (transform_fn *f, int n, double *a, double s)
{
  init (n, a);
  (*f) (n, a, s);
  check (n, a, s);
}

/* Check various combinations for enforcing correct ordering of 
   construct matches.  */
static void
f1 (int n, double* a, double s)
{
#pragma omp target teams
#pragma omp parallel  
#pragma omp metadirective					\
  when (construct={target}					\
	: for)							\
  default (error at(execution) message("f1 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

static void
f2 (int n, double* a, double s)
{
#pragma omp target teams
#pragma omp parallel
#pragma omp metadirective					\
  when (construct={teams, parallel}				\
	: for)							\
  default (error at(execution) message("f2 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

static void
f3 (int n, double* a, double s)
{
#pragma omp target teams
#pragma omp parallel
#pragma omp metadirective					\
  when (construct={target, teams, parallel}			\
	: for)							\
  default (error at(execution) message("f3 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

static void
f4 (int n, double* a, double s)
{
#pragma omp target teams
#pragma omp parallel
#pragma omp metadirective					\
  when (construct={target, parallel}				\
	: for)							\
  default (error at(execution) message("f4 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

static void
f5 (int n, double* a, double s)
{
#pragma omp target teams
#pragma omp parallel
#pragma omp metadirective					\
  when (construct={target, teams}				\
	: for)							\
  default (error at(execution) message("f5 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

/* Next batch is for things where the construct doesn't match the context.  */
static void
f6 (int n, double* a, double s)
{
#pragma omp target
#pragma omp teams
#pragma omp metadirective					\
  when (construct={parallel}					\
	: error at(execution) message("f6 match failed"))	\
  default (parallel for)
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

static void
f7 (int n, double* a, double s)
{
#pragma omp target
#pragma omp teams
#pragma omp metadirective					\
  when (construct={target, parallel}				\
	: error at(execution) message("f7 match failed"))	\
  default (parallel for)
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

static void
f8 (int n, double* a, double s)
{
#pragma omp target
#pragma omp teams
#pragma omp metadirective					\
  when (construct={parallel, target}				\
	: error at(execution) message("f8 match failed"))	\
  default (parallel for)
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

/* Next test choosing the best alternative when there are multiple
   matches.  */
static void
f9 (int n, double* a, double s)
{
#pragma omp target teams
#pragma omp parallel
#pragma omp metadirective					\
  when (construct={teams, parallel}				\
	: error at(execution) message("f9 match incorrect 1"))	\
  when (construct={target, teams, parallel}			\
	: for)							\
  when (construct={target, teams}				\
	: error at(execution) message("f9 match incorrect 2"))	\
  default (error at(execution) message("f9 match failed"))
  for (int i = 0; i < n; i++)
    a[i] = a[i] * s;
}

/* Note there are no tests for the matching the extended simd clause
   syntax, which is only useful for "declare variant".  */

#define N 10
#define S 2.0

int
main (void)
{
  double a[N];
  doit (f1, N, a, S);
  doit (f2, N, a, S);
  doit (f3, N, a, S);
  doit (f4, N, a, S);
  doit (f5, N, a, S);
  doit (f6, N, a, S);
  doit (f7, N, a, S);
  doit (f8, N, a, S);
  doit (f9, N, a, S);
}

/* All the error calls should be optimized away.  */
/* { dg-final { scan-tree-dump-not "__builtin_GOMP_error" "optimized" } } */
