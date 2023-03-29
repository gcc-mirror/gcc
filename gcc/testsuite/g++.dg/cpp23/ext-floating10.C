// P1467R9 - Extended floating-point types and standard names.
// { dg-do run { target { c++23 && float128_runtime } } }
// { dg-options "" }
// { dg-add-options float128 }

#ifndef WIDTH
#ifndef __STDCPP_FLOAT128_T__
#error Unexpected
#endif
#define WIDTH 128
#endif

#include "ext-floating7.C"
