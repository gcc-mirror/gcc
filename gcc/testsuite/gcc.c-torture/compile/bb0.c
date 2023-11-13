/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  return (a & 0xfff000) != 0;

}
