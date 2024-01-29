/* { dg-do compile } */

#define N 128

short x[N];
short y[N];

void
f1 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] & 15);
}

void
f2 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= ((y[i] & 7) + 8);
}

void
f3 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= ((y[i] & 7) ^ 11);
}

void
f4 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] < 15 ? y[i] : 15);
}

void
f5 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] >>= (y[i] < 15 ? y[i] : 1);
}

void
f6 (void)
{
  for (int i = 0; i < N; ++i)
    x[i] = 32768 >> (y[i] & 15);
}

/* { dg-final { scan-tree-dump {:11:[^\n]+can narrow to signed:16 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {:18:[^\n]+can narrow to signed:16 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {:25:[^\n]+can narrow to signed:16 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {:32:[^\n]+can narrow to signed:16 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {:39:[^\n]+can narrow to signed:16 without loss [^\n]+>>} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to unsigned:16 without loss [^\n]+>>} "vect" } } */
