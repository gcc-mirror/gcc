/* PR debug/83034 */
/* { dg-do compile } */
/* { dg-options "-funroll-loops -Ofast -g" } */

__attribute__((__simd__)) float expf (float);

void
foo (float *a, int x)
{
  for (; x; x++)
    a[x] = expf (x);
}
