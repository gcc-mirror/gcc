/* { dg-do compile } */
/* { dg-options "-O3 -march=cascadelake --param vect-partial-vector-usage=2" } */
/* { dg-final { scan-assembler-not "vpbroadcastm" } } */

double a[1024], b[1024];

void foo (int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = b[i] * 3.;
}
