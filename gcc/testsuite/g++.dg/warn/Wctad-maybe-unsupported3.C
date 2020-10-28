// { dg-do compile { target c++17 } }
// { dg-options "-Wctad-maybe-unsupported -Wsystem-headers" }

#include "Wctad-maybe-unsupported.h"

A a{42}; // { dg-warning "may not intend to support class template argument deduction" }
