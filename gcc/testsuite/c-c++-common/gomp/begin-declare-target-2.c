/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

extern int a;
#pragma omp begin declare target
#pragma omp declare target to (a)
#pragma omp end declare target
int b;
#pragma omp begin declare target to (b)		/* { dg-error "'to' is not valid for '#pragma omp begin declare target'" } */
#pragma omp end declare target
int c;
#pragma omp begin declare target link (c)	/* { dg-error "'link' is not valid for '#pragma omp begin declare target'" } */
#pragma omp end declare target
int m;
#pragma omp begin declare target device_type (host) device_type (any)	/* { dg-error "too many 'device_type' clauses" } */
#pragma omp end declare target
#pragma omp begin declare target
#pragma omp end declare target to (p)		/* { dg-error "expected end of line before .to." } */
