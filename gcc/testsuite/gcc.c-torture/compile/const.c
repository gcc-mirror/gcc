/* { dg-additional-options "-std=gnu89" } */

main (a)
{
  return a + (~0 - 240);
}
