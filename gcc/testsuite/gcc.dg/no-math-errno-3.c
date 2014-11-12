/* Test __NO_MATH_ERRNO__ is not defined with -fmath-errno.  */
/* { dg-do compile } */
/* { dg-options "-fmath-errno" } */

#ifdef __NO_MATH_ERRNO__
#error "__NO_MATH_ERRNO__ defined"
#endif
