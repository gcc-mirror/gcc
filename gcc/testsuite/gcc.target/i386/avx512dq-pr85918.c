/* PR target/85918 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512dq -mavx512vl -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */

#define N 1024

long long ll[N];
unsigned long long ull[N];
double d[N];

void ll2d (void)
{
  int i;

  for (i = 0; i < N; i++)
    d[i] = ll[i];
}

void ull2d (void)
{
  int i;

  for (i = 0; i < N; i++)
    d[i] = ull[i];
}

void d2ll (void)
{
  int i;

  for (i = 0; i < N; i++)
    ll[i] = d[i];
}

void d2ull (void)
{
  int i;

  for (i = 0; i < N; i++)
    ull[i] = d[i];
}
