/* { dg-additional-options "-std=gnu89" } */

foo (p)
     int *p;
{
  if ((int) p > 0)
    return 1;
  else
    return 0;
}
