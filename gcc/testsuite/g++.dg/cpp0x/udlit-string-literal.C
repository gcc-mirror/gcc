// { dg-do compile { target c++11 } }
// { dg-require-effective-target stdint_types }
// { dg-skip-if "requires hosted libstdc++ for string in udlit-string-literal.h" { ! hostedlib } }
// PR c++/55582

#include "udlit-string-literal.h"

using namespace my_string_literals;

decltype("Hello, World!"s) s;
#if !__cpp_char8_t == !__cpp_lib_char8_t
decltype(u8"Hello, World!"s) s8;
#endif
decltype(L"Hello, World!"s) ws;
decltype(u"Hello, World!"s) s16;
decltype(U"Hello, World!"s) s32;
