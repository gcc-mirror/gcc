/* Test that we diagnose the __FUNCTION__ and the __PRETTY_FUNCTION__
   predefined identifiers in pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu11 -pedantic" } */

void
foo (void)
{
  const char *s;
  s = __FUNCTION__; /* { dg-warning " ISO C does not support .__FUNCTION__. predefined identifier" } */
  s = __PRETTY_FUNCTION__; /* { dg-warning " ISO C does not support .__PRETTY_FUNCTION__. predefined identifier" } */
  s = __extension__ __FUNCTION__;
  s = __extension__ __PRETTY_FUNCTION__;
}
