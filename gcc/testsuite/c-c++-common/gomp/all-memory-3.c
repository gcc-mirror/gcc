typedef struct __attribute__((__aligned__ (sizeof (void *)))) omp_depend_t {
  char __omp_depend_t__[2 * sizeof (void *)];
} omp_depend_t;

omp_depend_t z;

void
foo (void)
{
  int x = 0, y = 0;
  #pragma omp task depend(out: omp_all_memory)
  ;
  #pragma omp task depend(inout: omp_all_memory)
  ;
  #pragma omp task depend(out: x, omp_all_memory, y)
  ;
  #pragma omp task depend(inout: omp_all_memory, y)
  ;
  #pragma omp task depend(out: x, omp_all_memory)
  ;
  #pragma omp depobj (z) depend (inout: omp_all_memory)
}
