/* { dg-do compile } */

int
f1 (int *x, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    res += x[i * 2] == 1 ? 2 : 3;
  return res;
}

int
f2 (int *x)
{
  int res = 0;
  for (int i = 0; i < 0x83; ++i)
    res += x[i * 2] == 1 ? 2 : 3;
  return res;
}

int
f3 (int *x, int n)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    res += x[i * 2] == 1 ? 2 : 3;
  return res + x[0x100];
}

int
f4 (int *x)
{
  int res = 0;
  for (int i = 0; i < 0x83; ++i)
    res += x[i * 2] == 1 ? 2 : 3;
  return res + x[0x100];
}

int
f5 (int *x, int n, int a)
{
  int res = 0;
  for (int i = 0; i < n; ++i)
    res += x[i * 2] == 1 ? 2 : 3;
  x[a] += 1;
  return res;
}

int
f6 (int *x, int a)
{
  int res = 0;
  for (int i = 0; i < 0x83; ++i)
    res += x[i * 2] == 1 ? 2 : 3;
  x[a] += 1;
  return res;
}
