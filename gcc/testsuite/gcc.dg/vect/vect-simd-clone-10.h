#pragma omp declare simd notinbranch
extern int foo (long int a, int b, int c);
#pragma omp declare simd notinbranch
extern long int bar (int a, int b, long int c);
