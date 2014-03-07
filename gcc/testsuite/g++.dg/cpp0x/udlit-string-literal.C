// { dg-do compile { target c++11 } }
// { dg-require-effective-target stdint_types }
// PR c++/55582

#include "udlit-string-literal.h"

using namespace my_string_literals;

decltype("Hello, World!"s) s;
decltype(u8"Hello, World!"s) s8;
decltype(L"Hello, World!"s) ws;
decltype(u"Hello, World!"s) s16;
decltype(U"Hello, World!"s) s32;
