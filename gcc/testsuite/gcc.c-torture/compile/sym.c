/* { dg-additional-options "-std=gnu89" } */

foo ()
{
  return (int) &foo;
}
