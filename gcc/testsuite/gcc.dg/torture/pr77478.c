/* { dg-do run } */
/* { dg-additional-options "-ffast-math" } */

static const float A[10] = {1};

float
foo(float *f, int n)
{
  int i, j;
  float a = 0, b = 0;
  for (i = n/2; i < n; i++)
    a += f[i]*.1f;
  for (i = n/2, j = 0; i < n; i++, j++)
    b += f[i]*A[j]+a*A[j];
  return b;
}

int main()
{
  float a[21] = {0};
  return foo(a+1, 20) + foo(a, 20);
}
