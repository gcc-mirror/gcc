/* Test __STDC_VERSION_STDATOMIC_H__ not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

#ifdef __STDC_VERSION_STDATOMIC_H__
#error "__STDC_VERSION_STDATOMIC_H__ defined"
#endif
