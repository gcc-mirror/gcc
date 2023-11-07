/* Test __STDC_VERSION_LIMITS_H__ in C23.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <limits.h>

#ifndef __STDC_VERSION_LIMITS_H__
#error "__STDC_VERSION_LIMITS_H__ not defined"
#endif

#if __STDC_VERSION_LIMITS_H__ != 202311L
#error "bad value of __STDC_VERSION_LIMITS_H__"
#endif
