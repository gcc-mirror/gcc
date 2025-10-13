// PR c++/122228
// { dg-do compile { target c++11 } }
// { dg-require-iconv "IBM1047" }
// { dg-options "-fexec-charset=IBM1047 -std=c++11" }

#include "func_constexpr.C"
