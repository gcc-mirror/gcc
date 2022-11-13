/* Test __STDC_VERSION_STDDEF_H__ not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stddef.h>

#ifdef __STDC_VERSION_STDDEF_H__
#error "__STDC_VERSION_STDDEF_H__ defined"
#endif
