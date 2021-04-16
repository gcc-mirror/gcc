/* PR target/99767 */
/* { dg-do compile } */
/* { dg-options " -O1 -fopenmp-simd -fno-tree-dce -march=armv8-a+sve" } */

int a[1024], b[1024];

void
foo (void)
{
  #pragma omp simd
  for (int i = 0; i < 1024; i++)
    if (b[i] > 23) {
      a[i] = b[i] + 1;
      int v = 1 / 0;	/* { dg-warning "division by zero" } */
    }
}
