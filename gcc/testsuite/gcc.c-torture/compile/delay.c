/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
{
  if (a == 1)
    goto foo1;
  if (a == 2)
    goto foo2;
 foo1:
  return 2;
 foo2:
  return 3;
}
