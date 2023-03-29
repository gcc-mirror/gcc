int i;

#pragma omp assumes no_openmp, absent (target, teams) holds (i < 32U) holds (i < 32U)
void
bar (void)
{
}

#pragma omp assumes no_openmp_routines

#pragma omp assumes no_parallelism

#pragma omp assumes absent (for)
void
fred (void)
{
}

#pragma omp assumes absent (atomic, barrier, cancel, cancellation point) absent (critical, depobj) \
		    absent (distribute, flush, loop, masked, master, nothing, ordered) \
		    absent (parallel, scan, scope, section, sections, simd, single, task) \
		    absent (taskgroup, taskloop, taskwait, taskyield)
void
foo (void)
{
}
