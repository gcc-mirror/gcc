/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  if ((unsigned) a < 234)
    return 1;
}
