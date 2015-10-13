/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void bar (int a[10][10][10]);
void
foo (int a[10][10][10], int **b, int x)
{
  int c[10][10][10];
  #pragma omp task depend(out: a[2:4][3:0][:7])	/* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp task depend(inout: b[:7][0:0][:0]) /* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp task depend(in: c[:][:][10:])	/* { dg-error "zero length array section" } */
    bar (c);
  #pragma omp task depend(out: a[2:4][3:0][:x])	/* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp task depend(inout: b[:x][0:0][:0]) /* { dg-error "zero length array section" } */
    bar (a);
  #pragma omp task depend(in: c[:][x-2:x][10:])	/* { dg-error "zero length array section" } */
    bar (c);
}
