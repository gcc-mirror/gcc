/* { dg-additional-options "-std=gnu89" } */

foo (long long *p, int a, int b)
{
  *(p + a + b) = 876243243874343LL;
}

bar (p, pp)
     long long *p, *pp;
{
  long long a;
  *p++ = a;
  fee (*p);
    *p++ = *pp--;
    *p++ = *pp--;
  return (int) p;
}
