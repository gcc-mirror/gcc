// P3348R4 - C++26 should refer to C23 not C17
// { dg-do run { target c++26 } }
// { dg-additional-options "-O2" }

#include "stdarg1.C"

// { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" "" { target *-*-* } 0 }
