/* Test for ICE arising from GCC target pragma.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

double
foo (double a)
{
  return __builtin_sqrt (a);
}
#pragma GCC target "no-powerpc-gpopt"
