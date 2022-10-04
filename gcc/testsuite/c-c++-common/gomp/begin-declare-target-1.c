/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

extern int a;
#pragma omp begin declare target device_type (host)
int b;
#pragma omp begin declare target device_type (any)
char d;
#pragma omp end declare target
#pragma omp end declare target
#pragma omp begin declare target
long c;
#pragma omp end declare target
#pragma omp declare target
int foo (void) { return 0; }
#pragma omp begin declare target device_type (any)
int bar (void) { return 0; }
#pragma omp end declare target
#pragma omp end declare target
#pragma omp begin declare target device_type (any)
int baz (void) { return 0; }
#pragma omp declare target
int qux (void) { return 0; }
#pragma omp end declare target
#pragma omp end declare target
