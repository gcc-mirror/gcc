// { dg-do compile { target c++11 } }

int i;

[[omp::directive (assumes no_openmp, absent (target, teams) holds (i < 32U) holds (i < 32U))]];
void
bar (void)
{
}

[[omp::directive (assumes no_openmp_routines)]];
[[omp::directive (assumes no_parallelism)]];
[[omp::directive (assumes absent (for))]];
void
fred (void)
{
}

[[omp::directive (assumes absent (atomic, barrier, cancel, cancellation point) absent (critical, depobj)
		    absent (distribute, flush, loop, masked, master, nothing, ordered)
		    absent (parallel, scan, scope, section, sections, simd, single, task)
		    absent (taskgroup, taskloop, taskwait, taskyield))]];
void
foo (void)
{
}
