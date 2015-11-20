/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void bar (int a[10][10][10]);
extern int f[][2];					/* { dg-error "has incomplete type" "" { target c++ } } */
extern int g[];						/* { dg-error "has incomplete type" "" { target c++ } } */
void
foo (int a[10][10][10], int **b, int x)
{
  int c[10][10][0];
  int d[0];
  char e[12];
  #pragma omp parallel reduction(+: a[:4][:0][:7])	/* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp parallel reduction(+: b[:7][0:0][:0])	/* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp parallel reduction(+: c[:][:][0:])	/* { dg-error "zero length array section|for unknown bound array type length expression must be specified" } */
    bar (a);
  #pragma omp parallel reduction(+: a[:4][:0][:x])	/* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp parallel reduction(+: b[:x][0:0][:0])	/* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp parallel reduction(+: c[:][:x][0:])	/* { dg-error "zero length array section|for unknown bound array type length expression must be specified" } */
    bar (a);
  #pragma omp parallel reduction(+: d)			/* { dg-error "is a zero size array" } */
    bar (a);
  #pragma omp parallel reduction(+: a[0:4])
    bar (a);
  #pragma omp parallel reduction(+: a[2:4])
    bar (a);
  #pragma omp parallel reduction(+: e[2:4])
    bar (a);
  #pragma omp parallel reduction(+: a[x:4])
    bar (a);
  #pragma omp parallel reduction(+: e[x:4])
    bar (a);
  #pragma omp parallel reduction(+: a[x:x])
    bar (a);
  #pragma omp parallel reduction(+: e[x:x])
    bar (a);
  #pragma omp parallel reduction(+: a[0.5:2])		/* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    bar (a);
  #pragma omp parallel reduction(+: a[0:2.5])		/* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    bar (a);
  #pragma omp parallel reduction(+: f[:][0:2])		/* { dg-error "for unknown bound array type length expression must be specified" } */
    bar (a);
  #pragma omp parallel reduction(+: a[:][0:10])		/* { dg-error "for pointer type length expression must be specified" } */
    bar (a);
  #pragma omp parallel reduction(+: a[:10][0:12])	/* { dg-error "above array section size" } */
    bar (a);
  #pragma omp parallel reduction(+: b[0:10][0:10])	/* { dg-error "array section is not contiguous" } */
    bar (a);
  #pragma omp parallel reduction(+: a[0:2][0:9])	/* { dg-error "array section is not contiguous" } */
    bar (a);
  #pragma omp parallel reduction(+: f)			/* { dg-error "has an incomplete type|invalid use of array with unspecified bounds" } */
    bar (a);
  #pragma omp parallel reduction(+: g)			/* { dg-error "has an incomplete type|invalid use of array with unspecified bounds" } */
    bar (a);
}
