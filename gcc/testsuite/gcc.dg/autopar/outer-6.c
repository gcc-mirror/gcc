/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

void abort (void);

int x[500][500];
int y[500];
int g_sum=0;

__attribute__((noinline))
void init (int i, int j)
{
  x[i][j]=1;
}

__attribute__((noinline))
void parloop (int N)
{
  int i, j;
  int sum;

  /* Outer loop reduction, outerloop is parallelized.  */ 
  sum=0;
  for (i = 0; i < N; i++)
  {
    for (j = 0; j < N; j++)
      y[i]+=x[i][j];
    sum += y[i];
  }
  g_sum = sum;
}

int main(void)
{
  int i,j;
  for (i = 0; i < 500; i++) 
    for (j = 0; j < 500; j++)
      init(i, j);
  
  parloop(500);

  return 0;
}


/* Check that outer loop is parallelized.  */
/* { dg-final { scan-tree-dump-times "parallelizing outer loop" 1 "parloops2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "parallelizing inner loop" 0 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 4 "optimized" { xfail *-*-* } } } */
