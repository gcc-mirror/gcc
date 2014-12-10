/* Test case to check if intrinsics and function specific target
   optimizations work together.  Check if the POPCNT specific intrinsics
   in included with popcntintrin.h get enabled by directly including
   popcntintrin.h  */

/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4.1 -mno-sse4.2 -mno-popcnt -std=gnu89" } */

#include <popcntintrin.h>

__attribute__((target("popcnt")))
long long foo(unsigned long long X)
{
    return _mm_popcnt_u64 (X);
}
