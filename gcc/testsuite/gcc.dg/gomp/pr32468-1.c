/* PR libgomp/32468 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-ompexp" } */

extern int printf (const char *, ...);
extern int omp_get_thread_num (void), omp_get_num_threads (void);
extern int bar (void);
extern int baz (const char *, ...);

void
f1 (void)
{
#pragma omp parallel
  {
    baz ("%d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
    #pragma omp sections
      {
	#pragma omp section
	printf ("section1 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
	#pragma omp section
	printf ("section2 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
      }
  }
}

void
f2 (void)
{
#pragma omp parallel
  {
    #pragma omp sections
      {
	#pragma omp section
	printf ("section1 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
	#pragma omp section
	printf ("section2 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
      }
    baz ("%d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
  }
}

void
f3 (void)
{
#pragma omp parallel
  {
    int bb = bar ();
    #pragma omp sections
      {
	#pragma omp section
	printf ("section1 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
	#pragma omp section
	printf ("section2 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
      }
  }
}

void
f4 (void)
{
  int i;
#pragma omp parallel
  {
    baz ("%d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
    #pragma omp for schedule (dynamic, 15)
    for (i = 0; i < 10000; i++)
      printf ("section1 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
  }
}

void
f5 (void)
{
  int i;
#pragma omp parallel
  {
    #pragma omp for schedule (dynamic, 15)
    for (i = 0; i < 10000; i++)
      printf ("section1 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
    baz ("%d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
  }
}

void
f6 (void)
{
  int i;
#pragma omp parallel
  {
    int bb = bar ();
    #pragma omp for schedule (runtime)
    for (i = 0; i < 10000; i++)
      printf ("section1 %d/%d\n", omp_get_thread_num (), omp_get_num_threads ());
  }
}

/* There should not be a GOMP_parallel_{loop,sections}* call.  */
/* { dg-final { scan-tree-dump-times "GOMP_parallel_loop" 0 "ompexp"} } */
/* { dg-final { scan-tree-dump-times "GOMP_parallel_sections" 0 "ompexp"} } */
