/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

#define N 1000

int A[N], B[N], C[N];
int main (void)
{
#pragma simd private (B) linear(B:1) /* { dg-error "applied to non-integral" } */
  for (int ii = 0; ii < N; ii++)
    {
      A[ii] = B[ii] + C[ii];
    }

#pragma simd private (B, C) linear(B:1) /* { dg-error "applied to non-integral" } */
  for (int ii = 0; ii < N; ii++)
    {
      A[ii] = B[ii] + C[ii];
    }

#pragma simd private (B) linear(C:2, B:1) /* { dg-error "applied to non-integral" } */
  for (int ii = 0; ii < N; ii++)
    {
      A[ii] = B[ii] + C[ii];
    }

#pragma simd reduction (+:B) linear(B:1) /* { dg-error "applied to non-integral" } */
  for (int ii = 0; ii < N; ii++)
    {
      A[ii] = B[ii] + C[ii];
    }

#pragma simd reduction (+:B) linear(B) /* { dg-error "applied to non-integral" } */
  for (int ii = 0; ii < N; ii++)
    {
      A[ii] = B[ii] + C[ii];
    }
  return 0;
}
