/* Test case to check if intrinsics and function specific target
   optimizations work together.  Check if an error is issued in
   -O0 mode when foo calls an intrinsic without the right target
   attribute.  */

/* { dg-do compile } */
/* { dg-options "-O0 -msse -mno-sse4.1 -mno-sse4.2" } */

#include <smmintrin.h>

__m128i foo(__m128i *V)
{
    return _mm_stream_load_si128(V); /* { dg-message "called from here" } */
}

/* { dg-prune-output ".*inlining failed.*" }  */
