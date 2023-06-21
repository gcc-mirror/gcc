/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx" } */

typedef double double4 __attribute__((vector_size(32)));

void fun(double * a, double * b)
{
  for (int i = 0; i < 1024; i+=4)
    *(double4*)&a[i] += *(double4 *)&b[i];
}

/* We don't want to spill but have both loads and stores lowered
   to supported SSE operations.  */
/* { dg-final { scan-assembler-not "movap\[sd\].*\[er\]sp" } } */
