/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  int b = 32;
  if (b & a)
    return 1;
  else
    return 0;
}
