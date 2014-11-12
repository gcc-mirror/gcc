/* Test __NO_MATH_ERRNO__ is defined with -fno-math-errno.  */
/* { dg-do compile } */
/* { dg-options "-fno-math-errno" } */

#ifndef __NO_MATH_ERRNO__
#error "__NO_MATH_ERRNO__ not defined"
#endif
