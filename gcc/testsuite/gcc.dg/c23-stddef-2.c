/* Test __STDC_VERSION_STDDEF_H__ in C23.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors -Wsystem-headers" } */

#include <stddef.h>
/* Simulate what glibc <time.h> does without actually including it.  */
#define __need_size_t
#define __need_NULL
#include <stddef.h>

#ifndef __STDC_VERSION_STDDEF_H__
#error "__STDC_VERSION_STDDEF_H__ not defined"
#endif

#if __STDC_VERSION_STDDEF_H__ != 202311L
#error "bad value of __STDC_VERSION_STDDEF_H__"
#endif
