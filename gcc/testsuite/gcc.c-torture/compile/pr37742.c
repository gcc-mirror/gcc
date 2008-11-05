void foo(int* __restrict__ p, int* q, int* p1, int *q1)
{
  int i;

  p = p1;
  q = q1;

  for (i = 0; i < 4; ++i)
    *++q = *++p + 1;
}

void bar(int* p, int* __restrict__ q, int* p1, int *q1)
{
  int i;

  p = p1;
  q = q1;

  for (i = 0; i < 4; ++i)
    *++q = *++p + 1;
}
