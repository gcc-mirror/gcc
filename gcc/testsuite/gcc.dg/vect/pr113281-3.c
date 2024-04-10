/* { dg-do compile } */

#define N 128

short x[N];
short y[N];

void
f1 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] < 30 ? y[i] : 30);
}

void
f2 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= ((y[i] & 15) + 2);
}

void
f3 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] < 16 ? y[i] : 16);
}

void
f4 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] = 32768 >> ((y[i] & 15) + 3);
}

/* { dg-final { scan-tree-dump {can narrow to signed:31 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to signed:18 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to signed:17 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to unsigned:19 without loss [^\n]+>>} "vect" } } */
