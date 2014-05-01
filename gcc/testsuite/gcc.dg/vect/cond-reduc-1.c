/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 512
int a[N];
int foo()
{
  int i, res = 0;
  for (i=0; i<N; i++)
  {
    if (a[i] != 0)
      res += 1;
  }
  return res;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

