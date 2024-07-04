/* { dg-additional-options "-std=gnu89" } */

foo ()
{
  return *(short *) 126;
}
