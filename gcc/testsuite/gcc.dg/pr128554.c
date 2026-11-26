/* PR c/128554 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wshift-count-negative" } */

unsigned
foo (void)
{
  const unsigned char c = 0;
  return __builtin_stdc_bit_floor (c);	/* { dg-bogus "left shift count is negative" } */
}
