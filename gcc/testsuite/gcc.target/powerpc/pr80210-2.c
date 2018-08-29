/* Test for ICE arising from GCC target pragma.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC target "no-powerpc-gpopt"
double
foo (double a)
{
  return __builtin_sqrt (a);
}
/* { dg-final { scan-assembler-not "fsqrt" } } */
