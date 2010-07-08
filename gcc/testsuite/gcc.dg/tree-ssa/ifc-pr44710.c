/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize" { target *-*-* } } */

#include <stdio.h>
#include <stdlib.h>

#define N 64
float arr[N];

__attribute__ ((noinline)) 
int foo (unsigned int n, float *min)
{
  unsigned int pos = 1;
  unsigned int i;
  float limit = N+N;

  for (i = 0; i < N; i++)
    if (arr[i] < limit)
      {
        pos = i + 1;
        limit = arr[i];
      }

  *min = limit;
  return pos;
}

int main (void)
{
  int i, pos;
  float min;

  for (i = 0; i < N; i++)
   arr[i] = (float)(i);

  arr[2] = -5.8;

  pos = foo (N, &min);
  if (pos != 3 || min != arr[2])
    abort ();

  return 0;
}

