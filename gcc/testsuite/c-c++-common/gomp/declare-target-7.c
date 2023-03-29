/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare target device_type (any)		/* { dg-error "directive with only 'device_type' clause" } */

void f1 (void) {}
#pragma omp declare target device_type (host) to (f1) device_type (nohost)	/* { dg-error "too many 'device_type' clauses" } */
#pragma omp declare target device_type (any) to (f1) device_type (any)		/* { dg-error "too many 'device_type' clauses" } */
