/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mmmx -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler "movswq" } } */
/* { dg-final { scan-assembler "pextrw" } } */

#include <xmmintrin.h>

long long int foo16(__m64 x) { return (short) _mm_extract_pi16(x, 3); }
