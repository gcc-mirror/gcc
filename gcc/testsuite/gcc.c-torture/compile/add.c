/* { dg-additional-options "-std=gnu89" } */

foo (a, b, p)
     int *p;
{
  return 34 + *p;
}
