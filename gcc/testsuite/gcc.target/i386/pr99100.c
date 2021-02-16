/* PR target/99100 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512f -fopenmp-simd -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler "_ZGVeN8v_myfunc" } } */
/* { dg-final { scan-assembler "_ZGVeN8v_sin" } } */

#pragma omp declare simd notinbranch
double sin (double x);
#pragma omp declare simd simdlen(8) notinbranch
__attribute__((const)) double myfunc (double x);

#define N 1024
__attribute__((__aligned__ (256))) double a[N], b[N], c[N];

void
foo ()
{
  for (int i = 0; i < N; i++)
    a[i] = myfunc (b[i]);
  for (int i = 0; i < N; i++)
    c[i] = sin (b[i]);
}
