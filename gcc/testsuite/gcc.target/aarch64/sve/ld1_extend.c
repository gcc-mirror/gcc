/* { dg-do compile } */
/* { dg-options "-O3 --param vect-partial-vector-usage=1" } */

void foo (signed char * __restrict__ a, signed char * __restrict__ b, short * __restrict__ c, int n)
{
    for (int i = 0; i < n; ++i)
      c[i] = a[i] + b[i];
}

/* { dg-final { scan-assembler-times {\tld1sb\t} 4 } } */
