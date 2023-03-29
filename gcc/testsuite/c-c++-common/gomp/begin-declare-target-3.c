/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp begin declare target
int a[] = { 1, 2, 3 };
extern int b[];			/* { dg-error "'b' in declare target directive does not have mappable type" } */
extern int c[];			/* { dg-error "'c' in declare target directive does not have mappable type" } */
extern int d[];			/* { dg-error "'d' in declare target directive does not have mappable type" } */
int d[3];
#pragma omp end declare target
int c[3];
#pragma omp begin declare target device_type (host)
int e[] = { 1, 2, 3 };
extern int f[];			/* { dg-error "'f' in declare target directive does not have mappable type" } */
extern int g[];			/* { dg-error "'g' in declare target directive does not have mappable type" } */
extern int h[];			/* { dg-error "'h' in declare target directive does not have mappable type" } */
int h[3];
#pragma omp end declare target
int g[3];
