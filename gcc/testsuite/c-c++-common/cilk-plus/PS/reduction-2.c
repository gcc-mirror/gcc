/* { dg-do run } */
/* { dg-options "-O3 -fcilkplus" } */

/* FIXME: This test has been xfailed until reductions are fixed.  */

#include <stdio.h>

#define ARRAY_SIZE  (256)
int a[ARRAY_SIZE];

__attribute__((noinline))
int addit (int *arr, int N)
{
  int s=0;
#pragma simd reduction (+:s)
  for (int i = 0; i < N; i++)
    s += arr[i];
  return s;
}

int main () {
  int i, s = 0, r = 0;
  for (i = 0; i < ARRAY_SIZE; i++)
    {
      a[i] = i;
    }

  s = addit (a, ARRAY_SIZE);

  for (i = 0; i < ARRAY_SIZE; i++) 
    r += i;

  if (s == r)
    return 0;
  return 1;
}
