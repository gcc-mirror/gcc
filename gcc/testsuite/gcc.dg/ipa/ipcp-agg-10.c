/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fno-inline" } */

int data1;

int callee1(int *v)
{
  if (*v < 2)
    return 0;
  else 
    {
      int t = data1;

      data1 = *v;
      *v = t;

      return 1;
    }
}

int __attribute__((pure)) callee2(int *v)
{
  if (*v < 2)
    return 0;
  else 
    {
      data1 = v[0] + v[2];

      return 1;
    }
}

int caller1(int c, int *r)
{
  int a = 1;

  if (c)
    return callee1(&a);
  else
    {
      *r = 2;
      return callee1(r);
    }
}

int data2[200];
int data3;

int __attribute__((const)) gen_cond(int);

int caller2(void)
{
  int i, j;
  int sum = 0;
  int a[8];

  a[0] = 3;
  for (i = 0; i < 100; i++)
    {
      if (gen_cond (i))
        continue;

      a[2] = 4;
      for (j = 0; j < 100; j++)
        {
          data2[i + j] = (i ^ j) + data3;

          sum += callee2(a);
        }
    }

  return sum;
}

/* { dg-final { scan-ipa-dump-times "offset: 0, type: int, CONST: 1" 1 "cp" } } */
/* { dg-final { scan-ipa-dump-times "offset: 0, type: int, CONST: 2" 1 "cp" } } */
/* { dg-final { scan-ipa-dump-times "offset: 0, type: int, CONST: 3" 1 "cp" } } */
/* { dg-final { scan-ipa-dump-times "offset: 64, type: int, CONST: 4" 1 "cp" } } */
