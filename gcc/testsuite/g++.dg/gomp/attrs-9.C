// { dg-do compile { target c++11 } }

[[omp::sequence (directive (requires, atomic_default_mem_order (seq_cst)))]];
[[omp::directive (declare reduction (plus: int: omp_out += omp_in) initializer (omp_priv = 0))]];
int a;
[[omp::directive (declare target (a))]];
int t;
[[omp::sequence (omp::directive (threadprivate (t)))]];
int b, c;
[[omp::directive (declare target, to (b), link (c))]];
[[omp::directive (declare target)]];
[[omp::directive (declare target)]];
int d;
[[omp::directive (end declare target)]];
[[omp::directive (end declare target)]];
[[omp::directive (nothing)]];
