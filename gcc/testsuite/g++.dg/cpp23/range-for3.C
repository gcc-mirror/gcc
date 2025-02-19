// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run { target c++11 } }
// Verify -frange-for-ext-temps works in earlier standards.
// { dg-additional-options "-frange-for-ext-temps" }

#define RANGE_FOR_EXT_TEMPS 1
#include "range-for1.C"
