/* { dg-do compile } */
/* { dg-options "-O2 -mmmx -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler "(movswl|cwtl)" } } */
/* { dg-final { scan-assembler "pextrw" } } */

#include <xmmintrin.h>

int foo16(__m64 x) { return (short) _mm_extract_pi16(x, 3); }
