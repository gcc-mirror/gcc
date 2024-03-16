/* { dg-additional-options "-std=gnu89" } */

foo (a, p)
     int *p;
{
  *p = a > 0;
}
