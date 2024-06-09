/* { dg-additional-options "-std=gnu89" } */

x ()
{}

y ()
{}

z (a, b)
{
  return (int) &a + (int) &b + (int) x + (int) z;
}
