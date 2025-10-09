// P3348R4 - C++26 should refer to C23 not C17
// { dg-do compile }

#include <stdarg.h>

#if __cplusplus >= 202400L
#ifndef __STDC_VERSION_STDARG_H__
#error __STDC_VERSION_STDARG_H__ not defined for C++26
#elif __STDC_VERSION_STDARG_H__ != 202311L
#error Unexpected __STDC_VERSION_STDARG_H__ value
#endif
#else
#ifdef __STDC_VERSION_STDARG_H__
#error __STDC_VERSION_STDARG_H__ defined for C++ < 26
#endif
#endif
