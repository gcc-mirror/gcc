/* PR target/69820 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -mno-avx512bw" } */

int a[100], b[100];
short c[100];

void
foo ()
{
  int i;
  for (i = 0; i < 100; ++i)
    b[i] = a[i] * (_Bool) c[i];
}
