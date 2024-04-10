/* { dg-do compile } */

#define N 128

short x[N];
short y[N];

void
f1 (void)
{
  for (int i = 0; i < N; ++i)
    {
      int a = y[i];
      int b = ~a;
      x[i] = b;
    }
}

void
f2 (void)
{
  for (int i = 0; i < N; ++i)
    {
      int a = y[i];
      int b = -a;
      x[i] = b;
    }
}

void
f3 (void)
{
  for (int i = 0; i < N; ++i)
    {
      int a = x[i];
      int b = a / y[i];
      x[i] = b;
    }
}

void
f4 (void)
{
  for (int i = 0; i < N; ++i)
    {
      int a = x[i];
      int b = a < y[i] ? a : y[i];
      x[i] = b;
    }
}

void
f5 (void)
{
  for (int i = 0; i < N; ++i)
    {
      int a = x[i];
      int b = a > y[i] ? a : y[i];
      x[i] = b;
    }
}

/* { dg-final { scan-tree-dump {can narrow to signed:17 without loss [^\n]+= -} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to signed:16 without loss [^\n]+= ~} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to signed:16 without loss [^\n]+ MIN_EXPR} "vect" } } */
/* { dg-final { scan-tree-dump {can narrow to signed:16 without loss [^\n]+ MAX_EXPR} "vect" } } */
