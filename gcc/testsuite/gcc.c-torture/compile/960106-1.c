/* { dg-additional-options "-std=gnu89" } */

f (a)
{
  return (a & 1) && !(a & 2 & 4);
}
