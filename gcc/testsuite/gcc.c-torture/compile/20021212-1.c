/* PR optimization/8334 */
/* Verify that GCC produces valid operands
   after simplifying an addition. */

void foo(int m, int n, double *f)
{
  int i, j, k = 1;

  for (j = 0; j < n; j++) {
    for (i = k; i < m; i++) {
      f[i] = (double) (i * j);
      f[i + j] = (double) ((i + 1) * j);
    }
  }
}
