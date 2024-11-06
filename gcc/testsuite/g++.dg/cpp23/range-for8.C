// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run { target c++11_only } }
// { dg-options "-std=c++11 -frange-for-ext-temps" }

#define RANGE_FOR_EXT_TEMPS 1
#include "range-for2.C"
