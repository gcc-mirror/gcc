/* { dg-additional-options "-std=gnu89" } */

long long f(s,r)
{
  return *(long long*)(s+r);
}

g(s,r)
{
  *(long long*)(s+r)=0;
}
