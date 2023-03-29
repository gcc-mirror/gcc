/* Test __STDC_VERSION_FLOAT_H__ not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <float.h>

#ifdef __STDC_VERSION_FLOAT_H__
#error "__STDC_VERSION_FLOAT_H__ defined"
#endif
