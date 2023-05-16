// { dg-do compile { target c++11 } }
// { dg-options "-Wno-error=narrowing -Wsystem-headers" }

// { dg-warning "narrowing" "" { target *-*-* } 2 }
#include "sys-narrow.h"
