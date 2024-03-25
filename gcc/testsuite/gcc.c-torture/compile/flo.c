/* { dg-additional-options "-std=gnu89" } */

foo (a)
     double a;
{
  double b = 0.0;

  return (a == 0);
}
