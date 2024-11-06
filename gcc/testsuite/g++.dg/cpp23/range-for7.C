// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run { target c++17_only } }
// { dg-options "-std=c++17 -frange-for-ext-temps" }

#define RANGE_FOR_EXT_TEMPS 1
#include "range-for1.C"
