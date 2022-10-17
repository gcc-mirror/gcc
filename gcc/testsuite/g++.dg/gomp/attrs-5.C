// { dg-do compile { target c++11 } }

typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

void
foo (int x)
{
  #pragma omp barrier
  [[omp::directive (barrier)]];
  #pragma omp parallel
  {
    #pragma omp cancel parallel
    [[omp::directive (cancellation point, parallel)]];
  }
  #pragma omp parallel
  {
    #pragma omp cancellation point parallel
    [[omp::directive (cancel parallel)]];
  }
  #pragma omp parallel
  {
    [[omp::directive (cancel, parallel)]];
    #pragma omp cancellation point parallel
  }
  omp_depend_t depobj;
  #pragma omp depobj(depobj) update(inout)
  [[omp::directive (depobj(depobj), destroy)]];
  #pragma omp flush
  [[omp::directive (flush)]];
  #pragma omp target enter data map (to: x)
  [[omp::directive (target exit data, map (from: x))]];
  [[omp::directive (target enter data, map (to: x))]];
  #pragma omp target exit data map (from: x)
  [[omp::directive (flush)]];
  #pragma omp target update to (x)
  [[omp::directive (flush)]];
  #pragma omp taskwait
  [[omp::directive (flush)]];
  #pragma omp taskyield
  [[omp::directive (flush)]];
  extern int t;
  #pragma omp threadprivate (t)
  [[omp::directive (flush)]];
}
