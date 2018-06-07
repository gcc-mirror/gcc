/* PR tree-optimization/84687 */
/* { dg-do compile } */
/* { dg-options "-Ofast" } */

int a[64], b;
double pow (double, double);
__attribute__((__simd__)) double exp (double);

void
foo (double x)
{
  int i;
  double c = exp (x);
  for (i = 0; i < 64; i++)
    {
      b = i;
      a[i] = pow (12.0, b) * pow (c, i);
    }
}
