/* Test __builtin_complex errors.  Verify it does not allow quiet
   creation of complex types in C90.  */
/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors" } */

void
f (void)
{
  __builtin_complex (0.0, 0.0); /* { dg-error "ISO C90 does not support complex types" } */
}
