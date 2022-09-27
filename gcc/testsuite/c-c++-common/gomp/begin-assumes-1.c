int i;

#pragma omp begin assumes no_openmp, absent (target, teams) holds (i < 32U) holds (i < 32U)
void
bar (void)
{
}
#pragma omp end assumes

#pragma omp begin assumes no_openmp_routines, contains (simd)
void
baz (int *a)
{
  #pragma omp simd
  for (int j = 0; j < i; j++)
    a[j] = j;
}
#pragma omp end assumes

#pragma omp begin assumes no_parallelism, contains (error)
void
qux (void)
{
  if (i >= 32)
    {
      #pragma omp error at (execution) message ("Should not happen")
    }
}
#pragma omp end assumes

#pragma omp begin assumes absent (for)
void
fred (void)
{
}
#pragma omp end assumes

#pragma omp begin assumes absent (atomic, barrier, cancel, cancellation point) absent (critical, depobj) \
			  absent (distribute, flush, loop, masked, master, nothing, ordered) \
			  absent (parallel, scan, scope, section, sections, simd, single, task) \
			  absent (taskgroup, taskloop, taskwait, taskyield)
void
foo (void)
{
}
#pragma omp end assumes
