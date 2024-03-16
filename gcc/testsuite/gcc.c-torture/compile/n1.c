/* { dg-additional-options "-std=gnu89" } */

foo (a, p)
     long long a;
     int *p;
{
  int b = (int)-a;
  if (b > 32)
    return 1;
  else
    return 0;
}
