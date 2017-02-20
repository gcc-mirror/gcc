/* PR sanitizer/79558 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=bounds" } */

void
fn1 (int n)
{
  int i, j;
  int x[2][0];
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      x[i][j] = 5;
}
