/* { dg-do run } */
/* { dg-options "-fcilkplus -O3" } */

#include <stdlib.h>

#define N 4

float f1[] =  { 2.0, 3.0,  4.0,  5.0 };
float f2[] =  { 1.0, 6.0, -1.0, -2.0 };
float res[] = { 3.0, 9.0,  3.0,  3.0 };

__attribute__((noinline))
void verify (float *sum)
{
  for (int i=0; i < N; ++i)
    if (sum[i] != res[i])
      abort ();
}

int main()
{
  float sum[N];
#pragma simd
  for (int i=0; i < N; ++i)
    sum[i] = f1[i] + f2[i];
  verify (sum);
  return 0;
}
