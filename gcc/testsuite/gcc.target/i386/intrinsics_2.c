/* Test case to check if intrinsics and function specific target
   optimizations work together.  Check by including immintrin.h  */

/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4.1" } */

#include <immintrin.h>

__attribute__((target("sse4.2")))
__m128i foo(__m128i *V)
{
    return _mm_stream_load_si128(V);
}
