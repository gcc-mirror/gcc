/* Test __STDC_VERSION_STDARG_H__ not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdarg.h>

#ifdef __STDC_VERSION_STDARG_H__
#error "__STDC_VERSION_STDARG_H__ defined"
#endif
