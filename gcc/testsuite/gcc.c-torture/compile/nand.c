/* { dg-additional-options "-std=gnu89" } */

nadn (a, b)
{
  return (~a) | (~b);
}
