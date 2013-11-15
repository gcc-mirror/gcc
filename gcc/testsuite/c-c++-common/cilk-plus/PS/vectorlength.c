/* { dg-do compile } */
/* { dg-options "-O3 -fcilkplus" } */

volatile int *a, *b, N;
typedef int tint;
struct someclass {
  int a;
  char b;
  int *p;
};

void foo()
{
#pragma simd vectorlength(4) vectorlength(8) /* { dg-error "too many 'vectorlength' clauses" } */
  for (int i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlength(3) /* { dg-error "must be a power of 2" } */
  for (int i=0; i < N; ++i)
    a[i] = b[i];
}
