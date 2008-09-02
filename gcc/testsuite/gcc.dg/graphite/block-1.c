/* { dg-options "-O2 -floop-block -fdump-tree-graphite-all" } */

#define MAX 8192

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

  return sum;
}

/* { dg-final { scan-tree-dump-times "Loop blocked" 3 "graphite"} } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
