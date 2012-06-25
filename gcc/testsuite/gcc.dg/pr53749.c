/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define N 1024
unsigned char a[N], b[N], c[N];

void f1(void)
{
  int i;
  for (i = 0; i < N; ++i)
    a[i] = b[i] * c[i];
}

void f2(void)
{
  int i;
  for (i = 0; i < N; ++i)
    a[i] = b[i] * 2;
}

void f3(void)
{
  int i;
  for (i = 0; i < N; ++i)
    a[i] = b[i] * 20;
}
