/* { dg-additional-options "-std=gnu89" } */

v (a, i)
     unsigned  *a, i;
{
  a++[i] = 0;
}
