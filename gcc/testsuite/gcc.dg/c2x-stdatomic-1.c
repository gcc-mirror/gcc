/* Test __STDC_VERSION_STDATOMIC_H__ in C2x.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#include <stdatomic.h>

#ifndef __STDC_VERSION_STDATOMIC_H__
#error "__STDC_VERSION_STDATOMIC_H__ not defined"
#endif

#if __STDC_VERSION_STDATOMIC_H__ != 202311L
#error "bad value of __STDC_VERSION_STDATOMIC_H__"
#endif
