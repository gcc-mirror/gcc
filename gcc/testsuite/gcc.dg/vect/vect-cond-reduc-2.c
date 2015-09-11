/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 512
int a[N], b[N];
void foo(int k)
{
  int i, res = 0;
  for (i=0; i<N; i++)
  {
    if (b[i] != 0)
      res += b[i];
  }
  a[k] = res;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

