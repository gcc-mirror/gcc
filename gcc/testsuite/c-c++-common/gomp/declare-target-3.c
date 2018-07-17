/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

#pragma omp declare target
int a[] = { 1, 2, 3 };
extern int b[];			/* { dg-error "'b' in declare target directive does not have mappable type" } */
extern int c[];			/* { dg-error "'c' in declare target directive does not have mappable type" } */
extern int d[];			/* { dg-error "'d' in declare target directive does not have mappable type" } */
int d[3];
#pragma omp end declare target
int c[3];
int e[] = { 1, 2, 3 };
#pragma omp declare target to (e)
extern int f[];
#pragma omp declare target to (f) /* { dg-error "'f' does not have a mappable type in 'to' clause" } */
extern int g[];
#pragma omp declare target to (g) /* { dg-error "'g' does not have a mappable type in 'to' clause" } */
int g[3];
extern int h[];
int h[3];
#pragma omp declare target to (h)

int i[] = { 1, 2, 3 };
int j[] = { 1, 2, 3 };
extern int k[];
extern int l[];
extern int m[];
extern int n[];
extern int o[];
extern int p[];
int k[3];
int l[3];
int q;

void
foo (void)
{
  #pragma omp target update to (q) to (i)
  #pragma omp target map (tofrom: j)
  ;
  #pragma omp target update from (q) from (k)
  #pragma omp target map (to: l)
  ;
  #pragma omp target update from (q) from (m)	/* { dg-error "'m' does not have a mappable type in 'from' clause" } */
  #pragma omp target map (from: n)		/* { dg-error "'n' does not have a mappable type in 'map' clause" } */
  ;
  #pragma omp target update to (q) to (o)	/* { dg-error "'o' does not have a mappable type in 'to' clause" } */
  #pragma omp target map (from: p)		/* { dg-error "'p' does not have a mappable type in 'map' clause" } */
  ;
}

int o[3];
int p[3];
