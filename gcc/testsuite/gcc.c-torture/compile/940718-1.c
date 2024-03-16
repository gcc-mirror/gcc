/* { dg-additional-options "-std=gnu89" } */

extern double log (double) __attribute__ ((const));

f (double x)
{
  for (;;)
    exp(log(x));
}
