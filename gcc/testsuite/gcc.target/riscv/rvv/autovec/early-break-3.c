/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=rv64gcv -mabi=lp64d -ftree-vectorize -std=gnu99" } */
/* { dg-final { scan-assembler-times {vms[lgen][teq]\.v[vxi]} 6 } } */
/* { dg-final { scan-assembler-times {vcpop\.m} 6 } } */

#define N 640
int a[N] = {0};
int b[N] = {0};

/* Should all generate compare + vcpop + branch.  */

void f1 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] > 0)
        break;
    }
}

void f2 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] >= 0)
        break;
    }
}

void f3 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] == 0)
        break;
    }
}

void f4 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] != 0)
        break;
    }
}

void f5 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] < 0)
        break;
    }
}

void f6 ()
{
  for (int i = 0; i < N; i++)
    {
      b[i] += a[i];
      if (a[i] <= 0)
        break;
    }
}
