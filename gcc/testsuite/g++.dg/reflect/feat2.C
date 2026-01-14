// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test feature test macros.

#include <version>

#ifndef __cpp_lib_reflection
#  error "__cpp_lib_reflection"
#elif __cpp_lib_reflection != 202506
#  error "__cpp_lib_reflection != 202506"
#endif
