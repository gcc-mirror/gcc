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

#pragma simd vectorlength(4) vectorlengthfor(int) /* { dg-error "too many 'vectorlength'" } */
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(int) vectorlengthfor(short int)
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(tint)
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(float)
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(_Complex double)
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(unsigned char)
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(struct someclass) /* { dg-error "type must be" } */
  for (i=0; i < N; ++i)
    a[i] = b[i];

#pragma simd vectorlengthfor(struct someclass *)
  for (i=0; i < N; ++i)
    a[i] = b[i];
}
