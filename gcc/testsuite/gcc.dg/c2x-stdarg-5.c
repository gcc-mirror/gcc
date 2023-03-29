/* Test __STDC_VERSION_STDARG_H__ in C2x.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#include <stdarg.h>

#ifndef __STDC_VERSION_STDARG_H__
#error "__STDC_VERSION_STDARG_H__ not defined"
#endif

#if __STDC_VERSION_STDARG_H__ != 202311L
#error "bad value of __STDC_VERSION_STDARG_H__"
#endif
