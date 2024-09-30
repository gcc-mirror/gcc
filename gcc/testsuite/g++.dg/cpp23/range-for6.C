// P2718R0 - Wording for P2644R1 Fix for Range-based for Loop
// { dg-do run { target { c++11 && c++20_down } } }
// { dg-options "-fno-range-for-ext-temps" }

#if __cplusplus <= 202002L
#define RANGE_FOR_EXT_TEMPS 0
#endif
#include "range-for2.C"
