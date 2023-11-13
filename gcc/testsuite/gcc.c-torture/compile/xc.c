/* { dg-additional-options "-std=gnu89" } */

foo (a, p)
     int *p;
{
  int b;

  a++;
  b = *p;
  if (a)
    return 1;
  return b;
}
