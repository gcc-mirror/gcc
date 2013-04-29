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
  int i;
#pragma simd vectorlength(4) vectorlength(8)
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlength(3) /* { dg-error "must be a power of 2" } */
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlength(4) vectorlength(8, 16)
  for (i=0; i < N; ++i)
    a[i] = b[i];
}
