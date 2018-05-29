/* PR target/85918 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512dq -mavx512vl -mprefer-vector-width=512 -fno-vect-cost-model -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 24 "vect" } } */

#define N 1024

long long ll[N] __attribute__((aligned (64)));
unsigned long long ull[N] __attribute__((aligned (64)));
float f[N] __attribute__((aligned (64)));
double d[N] __attribute__((aligned (64)));

void ll2d1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    d[i] = ll[i];
}

void ull2d1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    d[i] = ull[i];
}

void d2ll1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ll[i] = d[i];
}

void d2ull1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ull[i] = d[i];
}

void ll2f1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    f[i] = ll[i];
}

void ull2f1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    f[i] = ull[i];
}

void f2ll1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ll[i] = f[i];
}

void f2ull1 (void)
{
  int i;

  for (i = 0; i < 4; i++)
    ull[i] = f[i];
}

void ll2d2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    d[i] = ll[i];
}

void ull2d2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    d[i] = ull[i];
}

void d2ll2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ll[i] = d[i];
}

void d2ull2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ull[i] = d[i];
}

void ll2f2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    f[i] = ll[i];
}

void ull2f2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    f[i] = ull[i];
}

void f2ll2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ll[i] = f[i];
}

void f2ull2 (void)
{
  int i;

  for (i = 0; i < 8; i++)
    ull[i] = f[i];
}

void ll2d3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    d[i] = ll[i];
}

void ull2d3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    d[i] = ull[i];
}

void d2ll3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ll[i] = d[i];
}

void d2ull3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ull[i] = d[i];
}

void ll2f3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    f[i] = ll[i];
}

void ull2f3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    f[i] = ull[i];
}

void f2ll3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ll[i] = f[i];
}

void f2ull3 (void)
{
  int i;

  for (i = 0; i < 16; i++)
    ull[i] = f[i];
}
