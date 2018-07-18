/* PR c/79515 */
/* { dg-do compile } */
/* { dg-options "-Wdouble-promotion" } */

extern _Decimal64 x;
extern int i;

void
foo (void)
{
  if (x <= 2.0) /* { dg-error "mix operands" } */
    i++;
}
