#ifndef _GCC_WRAP_STDINT_H
#if __STDC_HOSTED__
# if defined __cplusplus && __cplusplus >= 201103L
#  undef __STDC_LIMIT_MACROS
#  define __STDC_LIMIT_MACROS
#  undef __STDC_CONSTANT_MACROS
#  define __STDC_CONSTANT_MACROS
# endif
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic" // include_next
# include_next <stdint.h>
#pragma GCC diagnostic pop
#else
# include "stdint-gcc.h"
#endif
#define _GCC_WRAP_STDINT_H
#endif
