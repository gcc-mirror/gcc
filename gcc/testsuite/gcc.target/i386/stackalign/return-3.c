/* { dg-do compile } */
/* { dg-skip-if "" { ! { ilp32 && dfp } } { "*" } { "" } } */
/* { dg-options "-msse -std=gnu99 -mpreferred-stack-boundary=2" } */

/* This compile only test is to detect an assertion failure in stack branch
   development.  */
_Decimal128 test (void)
{
  return 1234123412341234.123412341234dl;
}
