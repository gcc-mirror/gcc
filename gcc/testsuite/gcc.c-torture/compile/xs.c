/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
{
  for (b = 0; b < 10; b++)
    ;
  for (a = 0; a < 10; a++)
    ;
  a = b << 1;
  return a;
}
