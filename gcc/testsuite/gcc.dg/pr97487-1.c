/* PR middle-end/97487 */
/* { dg-do compile } */
/* { dg-options "-O2 --param max-rtl-if-conversion-unpredictable-cost=0" } */

long int __attribute__ ((simd))
foo (long int x, long int y)
{
  return x < 0 ? y : 0;
}
