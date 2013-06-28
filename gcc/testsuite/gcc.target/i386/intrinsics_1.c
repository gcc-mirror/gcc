/* Test case to check if intrinsics and function specific target
   optimizations work together.  Check by including x86intrin.h  */

/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4.1 -mno-sse4.2" } */

#include <x86intrin.h>

__attribute__((target("sse4.2")))
__m128i foo(__m128i *V)
{
    return _mm_stream_load_si128(V);
}
