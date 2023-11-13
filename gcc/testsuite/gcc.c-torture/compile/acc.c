/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  int b = a + 1;
  int c = (short) a;
  if (b)
    return b;
  return 1;
}
