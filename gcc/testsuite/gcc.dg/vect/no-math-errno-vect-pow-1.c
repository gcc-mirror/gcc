/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-fno-math-errno -ffinite-math-only -fno-signed-zeros" } */

double x[256];

void foo(void)
{
  int i;
  for (i=0; i<256; ++i)
    x[i] = __builtin_pow (x[i], 0.5);
}

