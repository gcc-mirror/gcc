// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run { target c++11 } }
// Verify -frange-for-ext-temps is not set by default in -std=gnu++* modes.
// { dg-options "" }

#define RANGE_FOR_EXT_TEMPS 0
#include "range-for2.C"
