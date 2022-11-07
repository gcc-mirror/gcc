void
foo (int i, int *a)
{
  #pragma omp assume no_openmp, absent (target, teams) holds (i < 32U) holds (i < 32U)
  ;
  #pragma omp assume no_openmp_routines, contains (simd)
  {
    #pragma omp simd
    for (int j = 0; j < i; j++)
      a[j] = j;
  }
  #pragma omp assume no_parallelism, contains (error)
  {
    if (i >= 32)
      {
	#pragma omp error at (execution) message ("Should not happen")
      }
  }
  #pragma omp assume absent (for)
  ;
  #pragma omp assume absent (atomic, barrier, cancel, cancellation point) absent (critical, depobj)
  ;
  #pragma omp assume absent (distribute, flush, loop, masked, master, nothing, ordered)
  ;
  #pragma omp assume absent (parallel, scan, scope, section, sections, simd, single, task)
  ;
  #pragma omp assume absent (taskgroup, taskloop, taskwait, taskyield)
  ;
}
