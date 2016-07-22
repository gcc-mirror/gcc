/* Test error for conditional expression between DFP and other
   floating operand.  */
/* { dg-do compile } */

_Decimal32 a;
float b;
int i;

void
f (void)
{
  (void) (i ? a : b); /* { dg-error "mix operands" } */
}
