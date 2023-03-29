/* Test __STDC_VERSION_STDINT_H__ not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors -ffreestanding" } */

#include <stdint.h>

#ifdef __STDC_VERSION_STDINT_H__
#error "__STDC_VERSION_STDINT_H__ defined"
#endif
