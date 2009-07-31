#define MAX 8192

void bar (void);

int main()
{
  int i, j;
  int sum = 0;
  int A[MAX * MAX];
  int B[MAX * MAX];

  bar ();

  for (i = 0; i < MAX; i++)
    for (j = 0; j < MAX; j++)
      {
	A[i*MAX + j] = j;
	B[i*MAX + j] = j;
      }

  for (i = 0; i < MAX; i++)
    for (j = 0; j < MAX; j++)
      A[i*MAX + j] += B[j*MAX + i];

  bar ();

  /* FIXME: For now, reductions are not handled by the code generation
     of graphite.  We have to bound the scop to the above loops.  */

  for(i = 0; i < MAX; i++)
    for(j = 0; j < MAX; j++)
      sum += A[i*MAX + j];

  return sum;
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 2 "graphite" { xfail *-*-* } } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
