// { dg-do compile { target c++26 } }
// { dg-require-iconv "IBM1047" }
// { dg-additional-options "-freflection -fexec-charset=IBM1047" }
// Test throwing std::meta::exception.

#include "eh5.C"
