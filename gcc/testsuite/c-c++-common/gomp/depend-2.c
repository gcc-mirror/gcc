/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void bar (int a[10][10][10]);
void
foo (int a[10][10][10], int **b)
{
  int c[10][10][10];
  #pragma omp task depend(out: a[2:4][3:][:7], b[1:7][2:8])
    bar (a);
  int i = 1, j = 3, k = 2, l = 6;
  #pragma omp task depend(in: a[++i:++j][++k:][:++l])
    bar (a);
  #pragma omp task depend(out: a[7:2][:][:], c[5:2][:][:])
  {
    bar (c);
    bar (a);
  }
}
