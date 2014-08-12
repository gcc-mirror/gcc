/* Test that we diagnose the __func__ predefined identifier in
   C90 pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void
foo (void)
{
  const char *s = __func__; /* { dg-error " ISO C90 does not support .__func__. predefined identifier" } */
}
