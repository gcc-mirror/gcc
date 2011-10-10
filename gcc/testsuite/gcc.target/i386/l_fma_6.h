
#ifndef l_fma_6
#define l_fma_6

void __attribute__((sseregparm))
test_noneg_add_noneg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = ((a[i] * b[i]) + c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_noneg_add_noneg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = ((a[i] * b[i]) + c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_noneg_add_neg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -((a[i] * b[i]) + c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_noneg_add_neg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -((a[i] * b[i]) + c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_noneg_sub_noneg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = ((a[i] * b[i]) - c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_noneg_sub_noneg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = ((a[i] * b[i]) - c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_noneg_sub_neg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -((a[i] * b[i]) - c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_noneg_sub_neg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -((a[i] * b[i]) - c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_neg_add_noneg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = (-(a[i] * b[i]) + c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_neg_add_noneg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = (-(a[i] * b[i]) + c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_neg_add_neg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -(-(a[i] * b[i]) + c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_neg_add_neg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -(-(a[i] * b[i]) + c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_neg_sub_noneg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = (-(a[i] * b[i]) - c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_neg_sub_noneg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = (-(a[i] * b[i]) - c[i]) * c[i] - b[i];
}

void __attribute__((sseregparm))
test_neg_sub_neg_add (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -(-(a[i] * b[i]) - c[i]) * c[i] + b[i];
}

void __attribute__((sseregparm))
test_neg_sub_neg_sub (TYPE *a, TYPE *b, TYPE *c, TYPE *d, int n)
{
  int i;
  for (i = 0; i < n; i++)
    d[i] = -(-(a[i] * b[i]) - c[i]) * c[i] - b[i];
}

#endif
