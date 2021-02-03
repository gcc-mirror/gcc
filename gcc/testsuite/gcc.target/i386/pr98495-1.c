/* { dg-do compile } */
/* { dg-options "-O2 -mmmx -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler-not "movswl" } } */
/* { dg-final { scan-assembler-not "movzwl" } } */
/* { dg-final { scan-assembler-not "cwtl" } } */
/* { dg-final { scan-assembler "pextrw" } } */

#include <xmmintrin.h>

unsigned int foo16(__m64 x) { return _mm_extract_pi16(x, 3); }
