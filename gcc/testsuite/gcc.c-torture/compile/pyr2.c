/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  return ((int *)0)[a];
}
