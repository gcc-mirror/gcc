/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-ffast-math" } */

float
frd (float *p, float *lastone)
{
  float sum = 0;
  for (; p <= lastone; p += 2)
    sum += p[0] + p[1];
  return sum;
}

/* { dg-final { scan-tree-dump "Starting SLP discovery of reduction chain" "vect" } } */
/* { dg-final { scan-tree-dump-not "SLP discovery of reduction chain failed" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
