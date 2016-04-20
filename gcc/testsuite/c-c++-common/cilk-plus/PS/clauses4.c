/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

#define N 1000

int B, C;
double D;

int main (void)
{
  #pragma simd linear (D:10)
  for (int ii = 0; ii < N; ii++)
    ;

  #pragma simd private (B) linear(B:1) /* { dg-error "more than once" } */
  for (int ii = 0; ii < N; ii++)
    ;

  #pragma simd private (B, C) linear(B:1) /* { dg-error "more than once" } */
  for (int ii = 0; ii < N; ii++)
    ;

  #pragma simd private (B) linear(C:2, B:1) /* { dg-error "more than once" } */
  for (int ii = 0; ii < N; ii++)
    ;

  #pragma simd reduction (+:B) linear(B:1) /* { dg-error "more than once" } */
  for (int ii = 0; ii < N; ii++)
    ;

  #pragma simd reduction (+:B) linear(B) /* { dg-error "more than once" } */
  for (int ii = 0; ii < N; ii++)
    ;

  return 0;
}
