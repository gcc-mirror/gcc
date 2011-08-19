/* Test __builtin_complex errors with DFP.  */
/* { dg-do compile } */

_Decimal32 a, b;

void
f (void)
{
  __builtin_complex (a, b); /* { dg-error "not of real binary floating-point type" } */
}
