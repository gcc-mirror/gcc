/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare target device_type (any)		/* { dg-warning "directive with only 'device_type' clauses ignored" } */

void f1 (void) {}
void f2 (void);
#pragma omp declare target to (f1) device_type (any) to (f2)

void f3 (void) {}
void f4 (void) {}
#pragma omp declare target device_type (host) to (f3)
#pragma omp declare target to (f4) device_type (nohost)

#pragma omp declare target
void f5 (void);
void f6 (void) {}
void f7 (void) {}
#pragma omp declare target to (f7)
void f8 (void) {}
#pragma omp declare target to (f8, f5)
#pragma omp declare target to (f5) to(f8)
#pragma omp declare target to (f8) device_type (host)
void f9 (void) {}
#pragma omp declare target to (f9) device_type (nohost)
#pragma omp declare target to (f9)
void f10 (void) {}
#pragma omp declare target device_type (any) to (f10)
void f11 (void) {}
#pragma omp end declare target

void f12 (void) {}
#pragma omp declare target device_type (any) to (f12)
#pragma omp declare target to (f12) device_type (host)
void f13 (void) {}
#pragma omp declare target device_type (host) to (f13)
#pragma omp declare target to (f13) device_type (nohost)
void f14 (void) {}
#pragma omp declare target device_type (nohost) to (f14)
#pragma omp declare target device_type (any) to (f14)
void f15 (void) {}
#pragma omp declare target device_type (host) to (f15) device_type (nohost)
void f16 (void) {}
#pragma omp declare target device_type (any) to (f15) device_type (any)
