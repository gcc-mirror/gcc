/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
     short a, b;
{
  return a < b;
}
