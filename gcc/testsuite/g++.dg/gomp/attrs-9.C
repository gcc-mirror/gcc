// { dg-do compile { target c++11 } }

int n1 = 0, n2 = 42;
[[omp::sequence (directive (requires, atomic_default_mem_order (seq_cst)))]];
[[omp::directive (declare reduction (plus: int: omp_out += omp_in) initializer (omp_priv = 0))]];
int a;
[[omp::directive (declare target (a))]];
int t;
[[omp::sequence (omp::directive (threadprivate (t)))]];
int b, c, e;
[[omp::directive (declare target, to (b), link (c), enter (e))]];
[[omp::directive (declare target)]];
[[omp::directive (declare target)]];
int d;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];
[[omp::directive (begin declare target, device_type (any))]];
[[omp::directive (begin declare target)]];
int f;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];
[[omp::directive (begin declare target device_type (host))]];
[[omp::directive (declare target)]];
int g;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];
[[omp::directive (declare target)]];
[[omp::directive (begin declare target, device_type (nohost))]];
int h;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];
[[omp::directive (nothing)]];
[[omp::directive (begin assumes no_openmp no_openmp_routines no_parallelism
				absent (atomic, barrier, cancel, cancellation point)
				absent (critical, depobj)
				absent (distribute, flush, loop, masked, master, nothing, ordered)
				absent (parallel, scan, scope, section, sections, simd, single, task)
				absent (taskgroup, taskloop, taskwait, taskyield)
				absent (target, teams, for, error) holds (n1 < n2))]];
void foo (void) {}
[[omp::directive (end assumes)]];
[[omp::directive (begin assumes, no_openmp, no_openmp_routines, no_parallelism,
				 absent (atomic, barrier, cancel, cancellation point),
				 absent (critical, depobj),
				 absent (distribute, flush, loop, masked, master, nothing, ordered),
				 absent (parallel, scan, scope, section, sections, simd, single, task),
				 absent (taskgroup, taskloop, taskwait, taskyield),
				 absent (target, teams, for, error), holds (n1 < n2))]];
[[omp::directive (begin assumes no_openmp)]];
void bar (void) {}
[[omp::sequence (omp::directive (end assumes), omp::directive (end assumes))]];
