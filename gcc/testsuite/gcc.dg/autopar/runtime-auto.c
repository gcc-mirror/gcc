/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops -fdump-tree-parloops2-details" } */

void abort (void);

#define N 1000

int a[N], b[N], c[N];

void
test_parallel_loop (void)
{
  int i;

  /* This loop should be auto-parallelized when -ftree-parallelize-loops
     (without =number) is used for runtime thread detection via OMP_NUM_THREADS.  */
  for (i = 0; i < N; i++)
    a[i] = b[i] + c[i];
}

int
main (void)
{
  int i;

  for (i = 0; i < N; i++)
    {
      b[i] = i;
      c[i] = i * 2;
    }

  test_parallel_loop ();

  for (i = 0; i < N; i++)
    {
      if (a[i] != b[i] + c[i])
	abort ();
    }

  return 0;
}

/* Check that the loop is parallelized with runtime thread detection.  */
/* { dg-final { scan-tree-dump "parallelizing" "parloops2" } } */

/* Check that "#pragma omp parallel" is generated.  */
/* { dg-final { scan-tree-dump "pragma omp parallel" "parloops2" } } */

/* Check that instead of generating a num_threads(x) clause, the compiler calls
   "__builtin_omp_get_num_threads" that will set the number of threads at
   program execution time.  */
/* { dg-final { scan-tree-dump "__builtin_omp_get_num_threads" "parloops2" } } */

