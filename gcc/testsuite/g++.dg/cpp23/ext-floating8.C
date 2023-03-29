// P1467R9 - Extended floating-point types and standard names.
// { dg-do run { target { c++23 && float32_runtime } } }
// { dg-options "" }
// { dg-add-options float32 }

#ifndef WIDTH
#ifndef __STDCPP_FLOAT32_T__
#error Unexpected
#endif
#define WIDTH 32
#endif

#include "ext-floating7.C"
