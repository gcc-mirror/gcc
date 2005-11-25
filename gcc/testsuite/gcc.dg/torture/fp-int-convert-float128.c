/* Test floating-point conversions.  __float128 type.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-xfail-if "" { i?86-*-* x86_64-*-* } { "*" } { "" } } */
/* { dg-options "" } */
/* { dg-options "-mmmx" { target { i?86-*-* && ilp32 } } } */
/* { dg-options "-mmmx" { target { x86_64-*-* && ilp32 } } } */

#include "fp-int-convert.h"

#define FLOAT128_MANT_DIG 113

int
main (void)
{
  TEST_I_F(signed char, unsigned char, __float128, FLOAT128_MANT_DIG);
  TEST_I_F(signed short, unsigned short, __float128, FLOAT128_MANT_DIG);
  TEST_I_F(signed int, unsigned int, __float128, FLOAT128_MANT_DIG);
  TEST_I_F(signed long, unsigned long, __float128, FLOAT128_MANT_DIG);
  TEST_I_F(signed long long, unsigned long long, __float128, FLOAT128_MANT_DIG);
  exit (0);
}
