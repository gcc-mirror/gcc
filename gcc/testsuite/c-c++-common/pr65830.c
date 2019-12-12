/* PR c/65830 */
/* { dg-do compile } */
/* { dg-options "-O -Wno-shift-count-negative -Wno-shift-count-overflow" } */

int
foo (int x)
{
  const int a = sizeof (int) * __CHAR_BIT__;
  const int b = -7;
  int c = 0;
  c += x << a;	/* { dg-bogus "10:left shift count >= width of type" } */
  c += x << b;	/* { dg-bogus "10:left shift count is negative" } */
  c += x >> a;	/* { dg-bogus "10:right shift count >= width of type" } */
  c += x >> b;  /* { dg-bogus "10:right shift count is negative" } */
  return c;
}
