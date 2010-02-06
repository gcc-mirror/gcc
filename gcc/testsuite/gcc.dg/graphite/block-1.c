/* { dg-require-effective-target size32plus } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define MAX 100

int main()
{
  int i, j;
  int sum = 0;
  int A[MAX * MAX];
  int B[MAX * MAX];

  for (i = 0; i < MAX; i++)
    for (j = 0; j < MAX; j++)
      {
	A[i*MAX + j] = j;
	B[i*MAX + j] = j;
      }

  for (i = 0; i < MAX; i++)
    for (j = 0; j < MAX; j++)
      A[i*MAX + j] += B[j*MAX + i];

  for(i = 0; i < MAX; i++)
    for(j = 0; j < MAX; j++)
      sum += A[i*MAX + j];

#if DEBUG
  fprintf (stderr, "sum = %d \n", sum);
#endif

  return sum != 990000;
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 2 "graphite" { xfail *-*-* } } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
