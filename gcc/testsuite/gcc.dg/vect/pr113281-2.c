/* { dg-do compile } */

#define N 128

short x[N];
short y[N];

void
f1 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= y[i];
}

void
f2 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] < 32 ? y[i] : 32);
}

void
f3 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] < 31 ? y[i] : 31);
}

void
f4 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] & 31);
}

void
f5 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= 0x8000 >> y[i];
}

void
f6 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= 0x8000 >> (y[i] & 31);
}

/* { dg-final { scan-tree-dump-not {can narrow[^\n]+>>} "vect" } } */
