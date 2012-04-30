// { dg-options "-std=c++0x -w -ftrack-macro-expansion=0" }

#include <limits.h>
extern constexpr int max_s = INT_MAX + 1;  // { dg-error "" }
extern constexpr unsigned max_u = UINT_MAX + 1u;  // OK
extern constexpr int abs_s = -INT_MIN;  // { dg-error "" } overflows on 2's complement machines
