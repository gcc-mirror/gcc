/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC push_options
#pragma GCC target ("arch=armv8-a+tme")
#ifndef __ARM_FEATURE_TME
#error "__ARM_FEATURE_TME is not defined but should be!"
#endif

#pragma GCC pop_options

#ifdef __ARM_FEATURE_TME
#error "__ARM_FEATURE_TME is defined but should not be!"
#endif

int
foo (int a)
{
  return a;
}
