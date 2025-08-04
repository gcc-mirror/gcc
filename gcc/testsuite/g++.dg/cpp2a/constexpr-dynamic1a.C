// Test that including <cxxabi.h>, whence the actual abi:__dynamic_cast
// is declared, doesn't affect our constexpr dynamic_cast handling.
// { dg-do compile { target c++20 } }

#include <cxxabi.h>
#include "constexpr-dynamic1.C"
