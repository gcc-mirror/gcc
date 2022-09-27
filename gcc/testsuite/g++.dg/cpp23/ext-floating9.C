// P1467R9 - Extended floating-point types and standard names.
// { dg-do run { target { c++23 && float64_runtime } } }
// { dg-options "" }
// { dg-add-options float64 }

#ifndef WIDTH
#ifndef __STDCPP_FLOAT64_T__
#error Unexpected
#endif
#define WIDTH 64
#endif

#include "ext-floating7.C"
