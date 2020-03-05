/* PR sanitizer/90954 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fsanitize=undefined" } */

float v;
int i;

void
foo (float x, float y)
{
  #pragma omp atomic
  v += x / y;
}

void
bar (int x, int y)
{
  #pragma omp atomic
  i += x / y;
}

void
baz (int x, int y)
{
  #pragma omp atomic
  i *= (x << y);
}
