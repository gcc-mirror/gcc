/* { dg-additional-options "-std=gnu89" } */

#define w 20
#define c 1

foo (a)
     unsigned a;
{
  return ((a & ((1 << w) - 1)) << c) > 0;
}
